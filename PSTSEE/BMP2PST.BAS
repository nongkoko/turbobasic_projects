def fnkalimat$(master$,start%,stops%)
 fnkalimat$ = mid$(master$,start%,stops%-start%+1)
end def
def fninstrrev(start%, sumber$, ygdicari$)
 local a%
 for a% = start% to 1 step -1
  if mid$(sumber$, a%, 1) = ygdicari$ then
   fninstrrev = a%
   exit def
  end if
 next a%
end def
def fnnamadasar$(any$)
 local slash%
 slash% = fninstrrev(len(any$),any$,"\")
 if slash% <> 0 then
  dot% = instr(slash%,any$,".")
  if dot% <> 0 then
   fnnamadasar$ = fnkalimat$(any$,slash%+1,dot%-1)
  else
   fnnamadasar$ = mid$(any$,slash%+1)
  end if
 else
  dot% = instr(any$,".")
  if dot% <> 0 then
   fnnamadasar$ = left$(any$,dot%-1)
  else
   fnnamadasar$ = any$
  end if
 end if
end def
perintah$ = command$
pathwindows$ = environ$("PATH")
pathwindows$
end
nama$ = fnnamadasar$(perintah$)
realcommand$ = "bmptool.exe "+perintah$+" b2p "+nama$+".pst"
? realcommand$
end
shell realcommand$