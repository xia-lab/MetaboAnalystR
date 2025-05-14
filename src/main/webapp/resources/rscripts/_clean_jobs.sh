#!/bin/bash

#######################################################################################
#################kill long running jobs################################################
#### add path of kill_job.sh to “/usr/bin/bash ”;                        ##############
#### usage: kill_job.sh                                                  ##############
#### developed by Peng Liu (rocpengliu@gmail.com) 2020-12-22             ##############
####                                                                     ##############
#######################################################################################


ps -eo pid,pmem,pcpu,etime,args --sort=start_time | grep -w "Rserve" | grep -v "grep" | tail -n +2 | sed -E "s/^ +//; s/ +/ /g" | cut -d" " -f-4 | while read id mem cpu ptim;
	do
		
		if echo ${ptim} | grep -q "-"; then # directly kill job > 1 day; with pattern "-" in the time format
			kill -9 ${id};
		else 
			#convert time to seconds
				flag=$(echo ${ptim} | grep -o ":" | wc -l );
			if(( $(echo "${flag} == 2" | bc -l) )); then
				totalTime=$(echo ${ptim} | awk -F: '{print ($1 * 3600) + ($2 * 60) + $3}'); 
			else
				totalTime=$(echo ${ptim} | awk -F: '{print ($1 * 60) + $2}'); 
			fi
		
			#check and kill jobs;
			if(( $(echo "${totalTime} >= 86400" | bc -l) )); then #kill jobs > 1 day; without pattern "-" in the time format
				kill -9 ${id};
			elif(( $(echo "${totalTime} >= 7200" | bc -l) )); then #kill jobs over 2 hours;
				kill -9 ${id};
			elif(( $(echo "${totalTime} >= 2400" | bc -l) && $(echo "${cpu} >= 99" | bc -l) )); then # kill jobs over 40 mins and 99% cpu
				kill -9 ${id};
			elif(( $(echo "${totalTime} >= 1200" | bc -l) && $(echo "${mem} >= 1" | bc -l) )); then # kill jobs over 20 mins and mem 4GB 1%
				kill -9 ${id};
			elif(( $(echo "${totalTime} >= 300" | bc -l) && $(echo "${mem} >= 4" | bc -l) )); then # kill jobs over 5 mins and mem 16GB 4%
				kill -9 ${id};
			elif(( $(echo "${totalTime} >= 120" | bc -l) && $(echo "${cpu} >= 1000" | bc -l) )); then #kill job > 1000 cpu and 2 min;
				kill -9 ${id};
			else
				:
			fi;
		fi;
	
	done;
