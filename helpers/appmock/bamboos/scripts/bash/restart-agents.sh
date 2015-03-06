#!/bin/sh

#####################################################################
# @author Rafal Slota
# @copyright (C): 2014 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# This script restarts Bamboo agents.
#####################################################################

ssh 172.16.67.229 "killall -KILL java ; service bamboo-agent restart" &
ssh 172.16.67.230 "killall -KILL java ; service bamboo-agent restart" &
ssh 172.16.67.231 "killall -KILL java ; service bamboo-agent restart" &
ssh 172.16.67.232 "killall -KILL java ; service bamboo-agent restart" &
ssh 172.16.67.233 "killall -KILL java ; service bamboo-agent restart" &
ssh 172.16.67.234 "killall -KILL java ; service bamboo-agent restart" &
ssh 172.16.67.249 "killall -KILL java ; service bamboo-agent restart" &

wait && echo "Done !"