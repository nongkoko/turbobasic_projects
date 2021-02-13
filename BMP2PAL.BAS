per$ = command$
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
def fninstrrev(start%, sumber$, ygdicari$)
 local a%
 for a% = start% to 1 step -1
  if mid$(sumber$, a%, 1) = ygdicari$ then
   fninstrrev = a%
   exit def
  end if
 next a%
end def
def fnkalimat$(master$,start%,stops%)
 fnkalimat$ = mid$(master$,start%,stops%-start%+1)
end def
def fnpathke$(berapa%, pathname$)
 local jmlkata%, temp$
 jmlkata% = fngeneration%(pathname$+" ")
 for katake% = 1 to jmlkata%-berapa%
  temp$ = temp$ + fnpath$(fnkatake$(katake%,pathname$,"\"))
 next katake%
 fnpathke$ = temp$
end def
def fnkatake$(kk,m$,p$)
 local starthu, stophu,pan%
 m$ = m$ + p$
 if kk = 1 then
  starthu = 1
  stophu  = fnjedake(1,m$,p$)-1
 else
  starthu = fnjedake(kk-1,m$,p$)+1
  stophu  = fnjedake(kk,m$,p$)-1
  call panstr(m$,pan%)
  if stophu <= -1 then stophu = pan%
 end if
 fnkatake$ = fnkalimat$(m$,starthu,stophu)
end def
sub PANSTR inline '(strin$,panjang%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &HA5, &H1F, &H5D
End Sub
def fnjedake(brp%,m$,p$)
local start%
 i.hrfke = brp%
 start% = 1
 do
  if i.hrfke <=0 then quitjedake
  start% = instr(start%,m$,p$) + 1
  i.hrfke = i.hrfke - 1
 loop
 quitjedake:
  fnjedake = start% - 1
end def
def fnpath$ (any$)
 if instr(any$, ".") = 0 then
  path$ = any$
  if (right$(path$,1) <> "\") and (path$ <> "") then path$ = path$ + "\"
  fnpath$ = path$
  exit def
 end if
 For a% = len(any$) to 1 step - 1
  if mid$(any$, a%,1) = "\" then exit for
 Next a%
 if a% = 0 then
  path$ = any$
 else
  path$ = left$(any$, a%)
 end if
 if (right$(path$,1) <> "\") and (path$ <> "") then path$ = path$ + "\"
 fnpath$ = path$
end def
def fngeneration%(pathname$)
 local jmlkata%, temp%, temp$
 if pathname$ <> "" then
  do
   temp% = instr(temp%+1, pathname$,"\")
   temp$ = mid$(pathname$,temp%+1,1)
   if (temp$ <> "\") and (temp$ <> "") then jmlkata% = jmlkata% + 1
  loop until temp% = 0
 end if
 fngeneration% = jmlkata%
end def
screen 0
shell "bmptool "+per$+" b2l "+fnpathke$(1,per$)+fnnamadasar$(per$)+".pal"