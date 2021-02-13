screen 0

def fnpadright$(tek$, maks%, with%)
 fnpadright$ = left$(tek$ + string$(maks%,with%),maks%)
end def
def fnpadleft$(tek$, maks%, with%)
 fnpadleft$ = right$(string$(maks%,with%) + tek$, maks%)
end def

do
 yeha% = inp(&H379)
 if yeha% <> yehax% then
  locate 23,1 : ? fnpadleft$(bin$(yeha%),8,48)
  yehax% = yeha%
 end if
loop until instat