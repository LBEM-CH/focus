C
C***************************************************************
	subroutine dat(date)
C*** returns date as character*8 dd-mm-yy
	character	date*8
	call  idate(imonth,iday,iyear)
	write(date(1:2),'(i2.2)') imonth
      	date(3:3) = '-'
	write(date(4:5),'(i2.2)') iday
        date(6:6) = '-'
	iyear = iyear - 1900
	write(date(7:8),'(i2.2)') iyear
	return
	end
C
C***************************************************************
	subroutine tim(time)
C*** returns time as character*8 hh:mm:ss
	character	date_time*24
	character	time*8
	call  fdate(date_time)
	time = date_time(12:19)
	return
	end
C
C*****************************************************************
      integer function chtoin(string)
C***
C*** function to convert character string to integer
      character*(*) string
      read(string,fmt='(i)') chtoin
      return
      end
C
C*****************************************************************
      character*(*) function intoch(number,nchars)
C***
C*** function to convert integer to character string
	character	ifm*4
	intoch = ' '
        inum = number
C*** calculate the number of numbers
        nchars = 0
        do while (inum.ge.1)
         nchars = nchars + 1
         inum = inum / 10
        end do
	if(nchars.gt.9) then
	 call squeak
	 write(6,'(
     *   '' Internal write failure - number too large'')')
	 return
	end if
C*** create format specification
	ifm = '(i'//char(ichar('0')+nchars)//')'
	write(intoch,fmt=ifm) number
	return
	end
C
C*******************************************************************
	subroutine squeak
	call putc(char(o'7'))
	return
	end
C
C***********************************************************
      character*(*) function upper(string)
      character*(*) string
C
C     function to convert alphabetic characters to uppercase
      nchars = len(string)
      do 100 i=1,nchars
       if(string(i:i).ge.'a'.and.string(i:i).le.'z')
     * string(i:i) = char(ichar(string(i:i)) - 32)
       upper(i:i) = string(i:i)
  100 continue
C***
      do 200 i=nchars+1,len(upper)
       upper(i:i) = ' '
  200 continue
      return
      end
