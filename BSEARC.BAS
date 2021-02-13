screen 0
jumlahelemen% = 256
dim array%(jumlahelemen%)

for a% = 1 to jumlahelemen%
 array%(a%) = a%
next a%

plusmin% = jumlahelemen%
plusmin% = ceil(plusmin%/2)

ygdicari% = 190

if ygdicari% > array%(jumlahelemen%) then
 ? "data tidak ada"
 end
end if

incr jumpen%
idx% = jumlahelemen% - plusmin%
plusmin% = ceil(plusmin%/2)

do
 if array%(idx%) > ygdicari% then
  idx% = idx% - plusmin%
 elseif array%(idx%) < ygdicari% then
  idx% = idx% + plusmin%
 end if
 plusmin% = ceil(plusmin%/2)
 incr jumpen%
loop until (ygdicari% = array%(idx%)) or idx% < 1
if idx% < 1 then
 ? "data tidak ada"
else
 ? "data ada pada index ke:";idx%
 ? "jumlah pencarian:";jumpen%
end if
end