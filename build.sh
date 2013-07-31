#!/usr/bin/env bash

PROJECT_DIR=$(cd ${0%/*} && pwd)
BUILD_DIR="build"

CPACK=`which cpack`
CMAKE=`which cmake`
if [ $? -gt 0 ]; then
	CPACK=`which cpack28`
	CMAKE=`which cmake28`
	if [ $? -gt 0 ]; then
		echo "Cannot find cmake binary. Please ensure its installed and available in PATH."
		exit 1
	fi
fi

mkdir -p $PROJECT_DIR/$BUILD_DIR
cd $PROJECT_DIR/$BUILD_DIR
$CMAKE .. && make 

if [ $? -gt 0 ]; then
	echo "Build failed :("
	exit 1
fi

$CPACK -C CPackConfig.cmake -G RPM
$CPACK -C CPackConfig.cmake -G DEB

echo ""
echo ""
echo "Fallowing packages are now available in $PROJECT_DIR/$BUILD_DIR directory: "
echo ""
for pkg in `ls *.rpm 2>/dev/null` 
do 
	echo $pkg
done

for pkg in `ls *.deb 2>/dev/null` 
do 
	echo $pkg
done 