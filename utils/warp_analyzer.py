#!/usr/bin/env python3

# coding=utf-8
"""Author: Bartek Kryza
Copyright (C) 2025 Onedata.org
This software is released under the MIT license cited in 'LICENSE.txt'

Minio Warp benchmark analysis script.
Analyzes CSV output from Warp benchmarks (supports zst compression).
Calculates statistics grouped by operation type.
"""

import pandas as pd
import numpy as np
import argparse
import sys
import zstandard as zstd
from pathlib import Path


def read_compressed_csv(file_path):
    """Read zst-compressed CSV file."""
    if str(file_path).endswith('.zst'):
        with open(file_path, 'rb') as compressed_file:
            dctx = zstd.ZstdDecompressor()
            with dctx.stream_reader(compressed_file) as reader:
                content = reader.read()
                # Convert bytes to string and create StringIO for pandas
                from io import StringIO
                df = pd.read_csv(StringIO(content.decode('utf-8')), sep='\t')
                # Strip whitespace from column names
                df.columns = df.columns.str.strip()
                return df
    else:
        df = pd.read_csv(file_path, sep='\t')
        # Strip whitespace from column names
        df.columns = df.columns.str.strip()
        return df


def calculate_statistics(df):
    """Calculate statistics grouped by operation type."""
    # Convert duration_ns to milliseconds for better readability
    df['duration_ms'] = df['duration_ns'] / 1_000_000
    
    # Ensure duration_ms is numeric
    df['duration_ms'] = pd.to_numeric(df['duration_ms'], errors='coerce')
    df = df.dropna(subset=['duration_ms'])
    
    # Group by operation type
    def safe_percentile(x, p):
        """Calculate percentile safely, handling edge cases."""
        x_clean = pd.to_numeric(x, errors='coerce').dropna()
        if len(x_clean) == 0:
            return np.nan
        return np.percentile(x_clean, p)
    
    stats = df.groupby('op')['duration_ms'].agg([
        'count',
        'mean',
        'std',
        'min',
        'max',
        lambda x: safe_percentile(x, 50),  # median
        lambda x: safe_percentile(x, 90),  # 90th percentile
        lambda x: safe_percentile(x, 95),  # 95th percentile
        lambda x: safe_percentile(x, 99),  # 99th percentile
    ]).round(2)
    
    # Rename lambda columns
    stats.columns = ['count', 'mean_ms', 'std_ms', 'min_ms', 'max_ms', 'p50_ms', 'p90_ms', 'p95_ms', 'p99_ms']
    
    return stats


def calculate_throughput_stats(df):
    """Calculate throughput statistics by operation type."""
    # Calculate throughput for operations with bytes > 0
    df_with_bytes = df[(pd.to_numeric(df['bytes'], errors='coerce') > 0)].copy()
    if len(df_with_bytes) == 0:
        return None
    
    # Ensure numeric types
    df_with_bytes['bytes'] = pd.to_numeric(df_with_bytes['bytes'], errors='coerce')
    df_with_bytes['duration_ns'] = pd.to_numeric(df_with_bytes['duration_ns'], errors='coerce')
    
    # Remove rows with invalid data
    df_with_bytes = df_with_bytes.dropna(subset=['bytes', 'duration_ns'])
    if len(df_with_bytes) == 0:
        return None
    
    # Convert bytes to MB and duration to seconds
    df_with_bytes['throughput_mbps'] = (df_with_bytes['bytes'] / (1024 * 1024)) / (df_with_bytes['duration_ns'] / 1_000_000_000)
    
    def safe_percentile(x, p):
        """Calculate percentile safely, handling edge cases."""
        x_clean = pd.to_numeric(x, errors='coerce').dropna()
        if len(x_clean) == 0:
            return np.nan
        return np.percentile(x_clean, p)
    
    throughput_stats = df_with_bytes.groupby('op')['throughput_mbps'].agg([
        'count',
        'mean',
        'std',
        'min',
        'max',
        lambda x: safe_percentile(x, 50),  # median
        lambda x: safe_percentile(x, 90),  # 90th percentile
        lambda x: safe_percentile(x, 95),  # 95th percentile
    ]).round(2)
    
    throughput_stats.columns = ['count', 'mean_mbps', 'std_mbps', 'min_mbps', 'max_mbps', 'p50_mbps', 'p90_mbps', 'p95_mbps']
    
    return throughput_stats


def print_results(duration_stats, throughput_stats, file_size_bytes=None):
    """Print formatted results."""
    print("=" * 80)
    if file_size_bytes is not None:
        # Convert bytes to human readable format
        if file_size_bytes >= 1024 * 1024:
            file_size_str = f"{file_size_bytes / (1024 * 1024):.1f} MB"
        elif file_size_bytes >= 1024:
            file_size_str = f"{file_size_bytes / 1024:.1f} KB"
        else:
            file_size_str = f"{file_size_bytes} bytes"
        print(f"OneS3 Warp benchmark, file size: {file_size_str}")
    else:
        print("OneS3 Warp benchmark")
    print("=" * 80)
    print()
    
    print("Duration Statistics by Operation Type:")
    print("=" * 80)
    print(duration_stats.to_string())
    print()
    
    if throughput_stats is not None and len(throughput_stats) > 0:
        print("Throughput Statistics by Operation Type (MB/s):")
        print("=" * 80)
        print(throughput_stats.to_string())
        print()
    
    print("Legend:")
    print("- count: Number of operations")
    print("- mean_ms: Average duration in milliseconds")
    print("- std_ms: Standard deviation in milliseconds")
    print("- min_ms/max_ms: Minimum/Maximum duration")
    print("- p50_ms/p90_ms/p95_ms/p99_ms: 50th/90th/95th/99th percentiles")
    if throughput_stats is not None and len(throughput_stats) > 0:
        print("- *_mbps: Throughput in megabytes per second")


def main():
    parser = argparse.ArgumentParser(description='Analyze Minio Warp benchmark CSV output')
    parser.add_argument('input_file', help='Path to CSV file (can be zst compressed)')
    parser.add_argument('--verbose', '-v', action='store_true', help='Verbose output')
    
    args = parser.parse_args()
    
    input_path = Path(args.input_file)
    if not input_path.exists():
        print(f"Error: File {input_path} does not exist", file=sys.stderr)
        sys.exit(1)
    
    try:
        # Read the data
        if args.verbose:
            print(f"Reading data from {input_path}...")
        
        df = read_compressed_csv(input_path)
        
        if args.verbose:
            print(f"Loaded {len(df)} records")
            print(f"Columns: {list(df.columns)}")
            if 'op' in df.columns:
                # Handle NaN values in op column
                unique_ops = df['op'].dropna().unique()
                print(f"Operations: {', '.join(map(str, unique_ops))}")
            if 'start' in df.columns and 'end' in df.columns:
                # Handle mixed types in date columns
                start_values = df['start'].dropna()
                end_values = df['end'].dropna()
                if len(start_values) > 0 and len(end_values) > 0:
                    # Convert to string to find min/max safely
                    start_str = start_values.astype(str)
                    end_str = end_values.astype(str)
                    print(f"Date range: {start_str.min()} to {end_str.max()}")
            print()
        
        # Check for required columns
        required_columns = ['op', 'duration_ns']
        missing_columns = [col for col in required_columns if col not in df.columns]
        if missing_columns:
            print(f"Error: Missing required columns: {missing_columns}", file=sys.stderr)
            print(f"Available columns: {list(df.columns)}", file=sys.stderr)
            sys.exit(1)
        
        # Clean data: remove rows with missing operation or duration
        initial_count = len(df)
        df = df.dropna(subset=['op', 'duration_ns'])
        if args.verbose and len(df) < initial_count:
            print(f"Removed {initial_count - len(df)} rows with missing op or duration_ns")
        
        # Convert duration_ns to numeric, coercing errors to NaN
        if args.verbose:
            print(f"Duration_ns dtype before conversion: {df['duration_ns'].dtype}")
            print(f"Sample duration_ns values: {df['duration_ns'].head()}")
        
        df['duration_ns'] = pd.to_numeric(df['duration_ns'], errors='coerce')
        # Remove rows where duration_ns couldn't be converted to numeric
        before_drop = len(df)
        df = df.dropna(subset=['duration_ns'])
        if args.verbose:
            print(f"Removed {before_drop - len(df)} rows with invalid duration_ns")
            print(f"Duration_ns dtype after conversion: {df['duration_ns'].dtype}")
        
        # Convert bytes to numeric as well
        if 'bytes' in df.columns:
            df['bytes'] = pd.to_numeric(df['bytes'], errors='coerce')
            df['bytes'] = df['bytes'].fillna(0)  # Fill NaN bytes with 0
            if args.verbose:
                print(f"Bytes dtype after conversion: {df['bytes'].dtype}")
        
        # Get file size from first operation with bytes > 0
        file_size_bytes = None
        operations_with_bytes = df[df['bytes'] > 0]
        if len(operations_with_bytes) > 0:
            file_size_bytes = operations_with_bytes['bytes'].iloc[0]
            if args.verbose:
                print(f"File size from first operation: {file_size_bytes} bytes")
        
        # Calculate statistics
        try:
            if args.verbose:
                print("Calculating duration statistics...")
            duration_stats = calculate_statistics(df)
            if args.verbose:
                print("Calculating throughput statistics...")
            throughput_stats = calculate_throughput_stats(df)
        except Exception as e:
            import traceback
            print(f"Error in statistics calculation: {e}", file=sys.stderr)
            if args.verbose:
                traceback.print_exc()
            sys.exit(1)
        
        # Print results
        print_results(duration_stats, throughput_stats, file_size_bytes)
        
    except Exception as e:
        import traceback
        print(f"Error processing file: {e}", file=sys.stderr)
        if args.verbose:
            traceback.print_exc()
        sys.exit(1)


if __name__ == '__main__':
    main()