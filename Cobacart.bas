screen 12

xpusat = 320
ypusat = 240

sub putc(x,y,warna)
shared xpusat, ypusat
   pset (xpusat+x,ypusat-y),warna
end sub

sub showruler
shared xpusat, ypusat
 line (0,ypusat)-(xpusat*2,ypusat),10
 line (xpusat,0)-(xpusat,ypusat*2),10
end sub

a = 1 : b = 24 : c = 15

if a<0 then
 langkah = 1
 start = -150
else
 langkah = -1
 start = 150
end if

call showruler

stasioner = -b/(2*a)
ymax = a*stasioner^2 + b*stasioner + c

for y = start to ymax step langkah
  call faktor(y,x1,x2)
  call putc(x1,y,12)
  call putc(x2,y,12)
next y

call faktor(start,x1,x2)
if x1>x2 then swap x1,x2
for x = x1 to x2
 call putc(x,fnmasuk(x),12)
next x

sub faktor(ketinggian,x1,x2)
shared a,b,c
x1 = (-(b/a) + abs((b/a)^2 - 4*((c-ketinggian)/a))^.5) / 2
x2 = (-(b/a) - abs((b/a)^2 - 4*((c-ketinggian)/a))^.5) / 2
end sub

def fnmasuk(x)
shared a,b,c
  fnmasuk = a*x^2 + b*x + c
end def

