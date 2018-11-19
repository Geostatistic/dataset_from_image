      subroutine chktitle(str,len)
c-----------------------------------------------------------------------
c
c                     Check for a Valid Title
c                     ***********************
c
c This subroutine takes the character string "str" of length "len" and
c blanks out all characters after the first back slash
c
c
c
c-----------------------------------------------------------------------
      character(len=*), intent(inout) :: str
      integer itrim
c
c Remove leading blanks:
      str=adjustl(str)
c
c find first three blanks and blank out remaining characters:
      itrim=index(str,'   ')
      if (itrim > 0) str(itrim:)=' '
c
c Find first back slash and blank out the remaining characters:
      itrim=index(str,'\')
      if (itrim > 0) str(itrim:)=' '
c
c Look for a five character pattern with -Titl...
      itrim=index(str,'-Titl')
      if (itrim > 0) str(itrim:)=' '
      itrim=index(str,'-titl')
      if (itrim > 0) str(itrim:)=' '
      itrim=index(str,'-TITL')
      if (itrim > 0) str(itrim:)=' '
      itrim=index(str,'-X la')
      if (itrim > 0) str(itrim:)=' '
      itrim=index(str,'-Y la')
      if (itrim > 0) str(itrim:)=' '
c
c Return with modified character string:
c
      return
      end
