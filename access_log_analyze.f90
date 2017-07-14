program analyze
implicit none

integer :: add1, add2, add3, add4, addfull
integer :: year, day, hour, minute, second, zone, date, secsum
integer :: hour1, minute1, second1, hour2, minute2, second2
character(len=3) :: month
character(len=20) :: ifcl, usrname
character(len=200) :: line
integer :: time(0:86399), host(1:10000,1:2), host2(1:10000,1:2)
integer :: hostnum, num, igmt, dtime, i, j, k, ios, ifper
integer :: dstart, dend

write(*,*) 'if you have multiple files'
write(*,*) 'input number of files'
read(*,*) num
write(*,*) 'if you want to specify the period for aggregation, input 1.'
write(*,*) 'if not, input 0'
read(*,*) ifper
if (ifper == 1) then
  write(*,*) 'input start date ( YYYYMMDD )'
  read(*,*) dstart
  write(*,*) 'input end date ( YYYYMMDD )'
  read(*,*) dend
else
  dstart=00000000
  dend=99999999
end if

time = 0
host = 0
igmt = 0
dtime = 3600
hostnum = 0

do k = 1, num
  if (num == 1) then
    open(111,file='/var/log/httpd/access_log')
  else if (num > 1) then
    write(line,*) k
    open(111,file='/var/log/httpd/access_log.'//trim(adjustl(line)))
  else
    write(*,*) 'error'
  end if
  open(222,file='time.log')
  open(333,file='host.log')

  do
    read(111,"(a200)",iostat=ios,end=999) line
    if (ios > 0) then
      write(*,*) 'error'
      stop
    end if
    i = index(line,'.') - 1
    read(line(1:i),*) add1 
    line = line(i+2:)
    i = index(line,'.') - 1
    read(line(1:i),*) add2 
    line = line(i+2:)
    i = index(line,'.') - 1
    read(line(1:i),*) add3 
    line = line(i+2:)
    i = index(line,' ') - 1
    read(line(1:i),*) add4 
    line = line(i+2:)
    read(line,"(a1,x,a1,x,a21,x,i5)") ifcl, usrname, line, zone
    read(line(2:3),*) day
    month = line(5:7)
    read(line(9:12),*) year
    read(line(14:15),*) hour
    read(line(17:18),*) minute
    read(line(20:21),*) second
    date = year * 10000
    select case (month)
      case('Jan')
        date = date + 100
      case('Feb')
        date = date + 200
      case('Mar')
        date = date + 300
      case('Apr')
        date = date + 400
      case('May')
        date = date + 500
      case('Jun')
        date = date + 600
      case('Jul')
        date = date + 700
      case('Aug')
        date = date + 800
      case('Sep')
        date = date + 900
      case('Oct')
        date = date + 1000
      case('Nov')
        date = date + 1100
      case('Dec')
        date = date + 1200
    end select
    date = date + day
    if ( date > dstart .and. date < dend) then
      secsum = second + minute * 60 + hour * 3600
      addfull = add1 * 2 ** (8 * 3) + add2 * 2 ** (8 * 2) + add3 * 2 ** (8 * 1) + add4 * 2 ** (8 * 0)

      time(secsum / dtime) = time(secsum / dtime) + 1
      j = hostnum + 1
      do i = 1, j
        if (host(i,1) == addfull) then
          host(i,2) = host(i,2) + 1
          exit
        else if (i > hostnum) then
          host(i,1) = addfull
          host(i,2) = 1
          hostnum = i
          exit
        else
          continue
        end if
      end do
    end if
  end do
999 continue
end do

do i = 0, 86399 / dtime
  secsum = i * dtime
  hour1 = secsum /3600
  secsum = secsum - hour1 * 3600
  minute1 = secsum / 60
  secsum = secsum - minute1 * 60
  second1 = secsum
  secsum = (i + 1) * dtime - 1
  hour2 = secsum /3600
  secsum = secsum - hour2 * 3600
  minute2 = secsum / 60
  secsum = secsum - minute2 * 60
  write(222,"(i2.2,':',i2.2,':',i2.2,'-',i2.2,':',i2.2,':',i2.2,',',i)") hour1, minute1, second1, hour2, minute2, second2, time(i)
end do
do i = 1, hostnum
  host2(i,1) = host(1,1)
  host2(i,2) = host(1,2)
  k = 1
  do j = 1, hostnum
    if (host2(i,2) < host(j,2)) then
      host2(i,1) = host(j,1)
      host2(i,2) = host(j,2)
      k = j
    end if
  end do
  host(k,2) = 0
end do

do i = 1, hostnum
  add1 = host2(i,1) / 2 ** (8 * 3)
  add2 = (host2(i,1) - add1 * 2 **(8 * 3)) / 2 ** (8 * 2)
  add3 = (host2(i,1) - add1 * 2 **(8 * 3) - add2 * 2 **(8 * 2)) / 2 ** (8 * 1)
  add4 = (host2(i,1) - add1 * 2 **(8 * 3) - add2 * 2 **(8 * 2) - add3 * 2 **(8 * 1)) / 2 ** (8 * 0)
  write(333,"(i3,'.',i3,'.',i3,'.',i3,',',i)") add1, add2, add3, add4, host2(i,2)
end do

end
