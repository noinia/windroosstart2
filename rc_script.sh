#!/bin/sh 

. /etc/rc.subr 

name="windroosstart" 
start_cmd="${name}_start" 
stop_cmd=":" 
windroosstart_user="www"

windroosstart_start() 
{
        cd /windroosstart
        su -m $windroosstart_user -c "./windroosstart Production | logger -p daemon.info -t windroosstart &"
        echo "Windroosstart started."
}

load_rc_config $name 
run_rc_command "$1" 






















