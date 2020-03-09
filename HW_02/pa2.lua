#!/usr/bin/env lua
-- Created by Bradley Morton for HW02 for Dr. Chappel's 331 class
-- Last edited Feb 11 2020
-- Contains four functions to be tested
-- Secret message is "You DID drink your Ovaltine, I hope."

local pa2 ={}


function pa2.filterTable(p,t)
	local validCases = {}
	for key, value in pairs(t) do
		if p(value) then
		validCases[key]=value
		end
	end
	return validCases
end



function pa2.concatMax(string, int)
	if(#string > int) then
		return ""
	else
		returnString = ""
		for i= 1, math.floor(int/#string), 1 do
			returnString=returnString .. string
		end
		return returnString
	end
end


function pa2.collatz(posInt)
	coroutine.yield(posInt)

	while posInt~=1 do
		if posInt%2==0 then
			posInt =posInt/2
		else 
			posInt = posInt*3+1
		end
		coroutine.yield(posInt)
	end
end



function pa2.allSubs(input)
	local length = string.len(input)
	local start, finish = 0, 0
	function iter(dummy)
		local locStart=start
		local locFinish = finish
		
		start=start+1
		finish=finish+1
		if finish-start>length then
			return nil
		end
		if finish >length then
			if locStart == 1 and locFinish >length then 
				return nil
			end
			currLen = finish-start
			start=1
			finish=start+currLen+1
		end

		return string.sub(input, locStart, locFinish)
		
		end


	return iter, nil, nil
end


return pa2