do
out &H378, &H04
delay 1
out &H378, &H00
delay 1
loop until instat