C***************************************************
C*** set of dummy routines to replace libhtml.for
C***************************************************
	subroutine	ccp4h_init_lib
	return
	end
	subroutine	ccp4h_summary_beg
	return
	end
	subroutine	ccp4h_pre_end
	return
	end
	subroutine	ccp4h_summary_end
	return
	end
	subroutine	ccp4h_html_close
	return
	end
	subroutine	ccp4h_pre_beg
	return
	end
C**************************************************************
        integer function numchars(text)
C***************************************************************
C*** function to return the number of non-blank characters in a 
C*** string and set remainder to ' '
        character*(*) text
        character zer0
        jchars = len(text)
        zer0 = char(0)
        do 100 i=jchars,1,-1
         numchars = i
         if(text(i:i) .eq. zer0) then
          text(i:i) = ' '
          go to 100
         end if
         if(text(i:i).ne.' ') return
  100   continue
        return
        end
