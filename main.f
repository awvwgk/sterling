* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1711
*     Copyright (C) 2017 awvwgk
*
*     This program is free software: you can redistribute it and/or 
*     modify it under the terms of the GNU General Public License as 
*     published by the Free Software Foundation, either version 3 of 
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program.  If not, see www.gnu.org/licenses/.
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1711
      module timings
      real*4, allocatable :: timings_cpu(:)
      integer,allocatable :: timings_wall(:)
      integer :: timings_max
      contains
      subroutine timing(i)
      call system_clock(timings_wall(i))
      call cpu_time(timings_cpu(i))
      end subroutine
      subroutine prtimings
      character(len=*),parameter :: f10 =
     .   '(2x,a,2x,''cpu-time:'',x,f7.3,x,''milliseconds'')'
      character(len=*),parameter :: f20 =
     .   '(2x,a,x,''wall-time:'',x,f7.3,x,''milliseconds'')'
      write(*,f10) 'total',
     .   (timings_cpu(timings_max)-timings_cpu(1))*1000
      write(*,f20) 'total',
     .   real(timings_wall(timings_max)-timings_wall(1))/10.0
      write(*,f10) 'exact',
     .   (timings_cpu(2)-timings_cpu(1))*1000
      write(*,f20) 'exact',
     .   real(timings_wall(2)-timings_wall(1))/10.0
      write(*,f10) 'first only',
     .   (timings_cpu(3)-timings_cpu(2))*1000
      write(*,f20) 'first only',
     .   real(timings_wall(3)-timings_wall(2))/10.0
      write(*,f10) 'first two',
     .   (timings_cpu(4)-timings_cpu(3))*1000
      write(*,f20) 'first two',
     .   real(timings_wall(4)-timings_wall(3))/10.0
      write(*,f10) 'first three',
     .   (timings_cpu(5)-timings_cpu(4))*1000
      write(*,f20) 'first three',
     .   real(timings_wall(5)-timings_wall(4))/10.0
      write(*,f10) 'all four',
     .   (timings_cpu(6)-timings_cpu(5))*1000
      write(*,f20) 'all four',
     .   real(timings_wall(6)-timings_wall(5))/10.0
      end subroutine prtimings
      end module timings
      program sterling
      use timings
      implicit none
      integer :: i
      real*8,dimension(100) :: res,res1,res2,res3,res4
      real*8,dimension(1:4) :: md,mad,rmsd
      real*8,parameter :: tpi = 8.0d0*atan(1.0d0)
      call prtime('S')
      timings_max = 7
      allocate( timings_cpu(timings_max),timings_wall(timings_max) ) 
      call timing(1)
      do i = 1, 100
         res(i) = logfactorial(i)
      enddo
      call timing(2)
      do i = 1, 100
         res1(i) = i*log(dble(i))
      enddo
      call timing(3)
      do i = 1, 100
         res2(i) = i*log(dble(i))-i
      enddo
      call timing(4)
      do i = 1, 100
         res3(i) = i*log(dble(i))-i+0.5d0*log(tpi*i)
      enddo
      call timing(5)
      do i = 1, 100
         res4(i) = i*log(dble(i))-i+0.5d0*log(tpi*i)+1.0d0/(14.0d0*i)
      enddo
      call timing(6)
      md(1)   = sum( res-res1 ) / 100
      mad(1)  = sum( abs(res-res1) ) / 100
      rmsd(1) = sqrt( sum( (res-res1)**2 ) ) / 100
      md(2)   = sum( res-res2 )  / 100
      mad(2)  = sum( abs(res-res2) )  / 100
      rmsd(2) = sqrt( sum( (res-res2)**2 ) ) / 100
      md(3)   = sum( res-res3 )  / 100
      mad(3)  = sum( abs(res-res3) )  / 100
      rmsd(3) = sqrt( sum( (res-res3)**2 ) ) / 100
      md(4)   = sum( res-res4 )  / 100
      mad(4)  = sum( abs(res-res4) )  / 100
      rmsd(4) = sqrt( sum( (res-res4)**2 ) ) / 100
      print'(''MD:'',2x,4f10.6)',md
      print'(''MAD:'',x,4f10.6)',mad
      print'(''RMSD:'', 4f10.6)',rmsd
      call prtime('E')
      call timing(timings_max)
      call prtimings
      call terminate(0)
      contains
      pure recursive function logfactorial(n) result(res)
      integer,intent(in) :: n
      real*8 :: res
      if (n.eq.0) then
         res = 0.0d0
      else
         res = log(dble(n))+logfactorial(n-1)
      endif
      end function logfactorial
      subroutine raise(mode,message)
      character,       intent(in) :: mode
      character(len=*),intent(in) :: message
      select case(mode)
      case('W','w')
      print'(''#WARNING!'',x,a)',message
      case('E','e')
      print'(''#ERROR!'',x,a)',  message
      call terminate(1)
      end select
      end subroutine raise
      subroutine terminate(signal)
      integer,intent(in) :: signal
      select case(signal)
      case(0)
      stop         'normal termination of sterling'
      case default
      error stop 'abnormal termination of sterling'
      end select
      end subroutine terminate
      subroutine prtime(mode)
      character,intent(in) :: mode
      character(len=:),allocatable :: i
      character(len=8)  :: d
      character(len=10) :: t
      character(len=5)  :: z
      integer :: v(8)
      select case(mode)
      case('S','s')
      i = 'started run on'
      case('E','e')
      i = 'finished run on'
      case default
      i = 'current time:'
      end select
      call date_and_time(d,t,z,v)
      write(*,'(''*''x,a,x,a,''/'',a,''/'',a,x,''at'',x,'//
     .   'a,'':'',a,'':'',a)') i,d(:4),d(5:6),d(7:),t(:2),t(3:4),t(5:)
      end subroutine prtime
      end program sterling
