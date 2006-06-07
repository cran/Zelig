      SUBROUTINE difp(nr,nc,np,x,y,p,const,diff)
c     input: nr,nc,np,x,y,p
c      output: penalized ls criterion
      implicit real*8 (a-h,o-z)
      parameter(maxr=10,maxc=10,maxn=1000)
      dimension x(np,nr),y(np,nc),gamma(maxr,maxc-1),
     & expo(maxr,maxc-1,maxn),eb(maxr,maxc-1,maxn),
     & p(nr*(nc-1)),fmean(maxn,maxc-1),yy(maxn*(maxc-1)),
     & fmm(maxn*(maxc-1))
      ncount=0
      do 5 i=1,nr
         do 5 j=1,nc-1
            ncount=ncount+1
            gamma(i,j)=p(ncount)
  5   continue
      do 10 k=1,np
         do 10 i=1,nr
            do 10 j=1,nc-1
               expo(i,j,k)=dexp(gamma(i,j))
  10  continue
      do 15 k=1,np
         do 25 i=1,nr
            s=0.0d0
            do 20 j=1,nc-1
               s=s+expo(i,j,k)
  20        continue
            do 30 j=1,nc-1
               eb(i,j,k)=expo(i,j,k)/(s+1.0d0)
  30        continue
  25     continue
  15  continue
      do 35 k=1,np
         do 40 j=1,nc-1
            s=0.0d0 
            do 45 i=1,nr
               s=s+x(k,i)*eb(i,j,k)
  45        continue
            fmean(k,j)=s
  40     continue
  35  continue
      do 50 j=1,nc-1
         do 50 k=1+(j-1)*np,j*np
            yy(k)=y(k-(j-1)*np,j)
            fmm(k)=fmean(k-(j-1)*np,j)
  50  continue
      diff=0.0d0
      do 55 i=1,np*(nc-1)
         diff=diff+(yy(i)-fmm(i))**2
  55  continue
c     add penalty
      pen=0.0d0
      do 60 i=1,nr
         do 60 j=1,nc-1
            pen=pen+gamma(i,j)**2
  60  continue
      diff=diff+const*pen
      return
      end
