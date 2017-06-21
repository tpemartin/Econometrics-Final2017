!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! authors: hyun hong
! purpose: local linear matching
!          use biweight kernel to meet compact condition
! date: 1 july 2004
! modified: 2 aug 2004
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! declare modules
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module expn

  real :: weight,psb,psa,cd,video,adm,toy,rd,sport
  real :: h,kwgtrcdb,kwgctcda,kwgctcdb
  integer :: rstat,ind

end module expn

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the main program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program ddm

  use expn
  implicit none

  real :: sumcd,sumwgt,ddmcd,ddmcd2
  real :: scdta,scdtb,scdca,scdcb
  real :: cdta,cdtb,cdca,cdcb
  real :: temp
  integer :: i

  ! open output files
  open (unit=15,file="llm_bootstrap.dat")

  ! open all input files
  open (unit=1,file="treatment_after.dat")
  open (unit=2,file="control_after.dat")
  open (unit=3,file="treatment_before.dat")
  open (unit=4,file="control_before.dat")

  ! bandwidth
  h = .07

  !**** treatment group after ****!
  ! initialize counters
  rstat = 1
  sumcd = 0
  sumwgt = 0

  scdta = 0
  scdtb = 0
  scdca = 0
  scdcb = 0


  do while (rstat.ne.-1)
     read (unit=1,*,IOSTAT=rstat) weight,psb,psa,cd

     call readtrb        ! weighted mean for treatment before 
     
     if (ind.ne.1) then
        call readctra       ! weighted mean for control after
        
        if (ind.ne.1) then
           call readctrb       ! weighted mean for control before
           
           if (ind.ne.1) then
              sumwgt = sumwgt + weight
              sumcd = sumcd + weight*(cd - kwgtrcdb - kwgctcda + kwgctcdb)
              
              scdta = scdta + weight*cd
              scdtb = scdtb + weight*kwgtrcdb
              scdca = scdca + weight*kwgctcda
              scdcb = scdcb + weight*kwgctcdb                
           end if
        end if
     end if

  end do

  ! LLM for treatment after
  ddmcd = sumcd/sumwgt

  cdta = scdta/sumwgt
  cdtb = scdtb/sumwgt
  cdca = scdca/sumwgt
  cdcb = scdcb/sumwgt

  ddmcd2 = cdta - cdtb - cdca + cdcb

  write(15,*) ddmcd,cdta,cdtb,cdca,cdcb,ddmcd2

end program ddm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! reads in the data for a control group and compute kernel-weighted mean
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine readctra

    use expn
    implicit none

    integer :: rstat2,tmp
    real :: wgt,pbc,pac,cdc,videoc,admc,toyc,rdc,sportc
    real :: kernel,tempa,tempb,num,den
    real :: G,GPa,GPb,GPa2,GPb2,GPaPb,GY,GPaY,GPbY
      
    ! initialize value
    G = 0     ! sum of kernel weight
    GPa = 0   ! sum of kernel*(Paj-Pai)
    GPb = 0   ! sum of kernel*(Pbj-Pbi)
    GPa2 = 0  ! sum of kernel*(Paj-Pai)**2
    GPb2 = 0  ! sum of kernel*(Pbj-Pbi)**2
    GPaPb = 0 ! sum of kernel*(Paj-Pai)*(Pbj-Pbi)
    GY = 0    ! sum of kernel*CD
    GPaY = 0  ! sum of kernel*(Paj-Pai)*CD
    GPbY = 0  ! sum of kernel*(Pbj-Pbi)*CD

    do while (rstat2.ne.-1)
       read (unit=2,*,IOSTAT=rstat2) wgt,pbc,pac,cdc

       ! two-dimensional biweight kernel
       tempb = (pbc-psb)/h
       tempa = (pac-psa)/h

       if ((abs(tempa).lt.1).and.(abs(tempb).lt.1)) then
          kernel = ((tempa**2-1)**2)*((tempb**2-1)**2)*225/256
       else
          kernel = 0
       end if

       G = G + kernel
       GPa = GPa + kernel*(pac-psa)
       GPb = GPb + kernel*(pbc-psb)
       GPa2 = GPa2 + kernel*(pac-psa)**2
       GPb2 = GPb2 + kernel*(pbc-psb)**2
       GPaPb = GPaPb + kernel*(pac-psa)*(pbc-psb)
       GY = GY + kernel*cdc
       GPaY = GPaY + kernel*(pac-psa)*cdc
       GPbY = GPbY + kernel*(pbc-psb)*cdc

    end do
 
    ind = 0   ! indicator for whether sumwgt = 0

    ! local linear weighted average of CD expenditures for control
    den = G*GPa2*GPb2+2*GPa*GPaPb*GPb-G*(GPaPb)**2-GPa2*(GPb)**2-GPb2*(GPa)**2
    num = GPa2*GPb2*GY-GY*(GPaPb)**2-GPa*GPb2*GPaY+GPb*GPaPb*GPaY+GPa*GPaPb*GPbY-GPb*GPa2*GPbY
    if ((den*num).gt.0) then
       kwgctcda = num/den
    else if (num.eq.0) then
       kwgctcda = 0
    else
       ind = 1
    end if

    ! rewind if it's the end of file
    if (rstat2.eq.-1) then 
       rewind (unit=2)
    end if
    
    ! reset the indicator for the end of file
    rstat2 = 1

end subroutine readctra


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! reads in the data for a control group and compute kernel-weighted mean
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine readctrb

    use expn
    implicit none

    integer :: rstat2,tmp
    real :: wgt,pbc,pac,cdc,videoc,admc,toyc,rdc,sportc
    real :: kernel,tempa,tempb,num,den
    real :: G,GPa,GPb,GPa2,GPb2,GPaPb,GY,GPaY,GPbY
      
    ! initialize value
    G = 0     ! sum of kernel weight
    GPa = 0   ! sum of kernel*(Paj-Pai)
    GPb = 0   ! sum of kernel*(Pbj-Pbi)
    GPa2 = 0  ! sum of kernel*(Paj-Pai)**2
    GPb2 = 0  ! sum of kernel*(Pbj-Pbi)**2
    GPaPb = 0 ! sum of kernel*(Paj-Pai)*(Pbj-Pbi)
    GY = 0    ! sum of kernel*CD
    GPaY = 0  ! sum of kernel*(Paj-Pai)*CD
    GPbY = 0  ! sum of kernel*(Pbj-Pbi)*CD

    do while (rstat2.ne.-1)
       read (unit=4,*,IOSTAT=rstat2) wgt,pbc,pac,cdc

       ! two-dimensional biweight kernel
       tempb = (pbc-psb)/h
       tempa = (pac-psa)/h

       if ((abs(tempa).lt.1).and.(abs(tempb).lt.1)) then
          kernel = ((tempa**2-1)**2)*((tempb**2-1)**2)*225/256
       else
          kernel = 0
       end if

       G = G + kernel
       GPa = GPa + kernel*(pac-psa)
       GPb = GPb + kernel*(pbc-psb)
       GPa2 = GPa2 + kernel*(pac-psa)**2
       GPb2 = GPb2 + kernel*(pbc-psb)**2
       GPaPb = GPaPb + kernel*(pac-psa)*(pbc-psb)
       GY = GY + kernel*cdc
       GPaY = GPaY + kernel*(pac-psa)*cdc
       GPbY = GPbY + kernel*(pbc-psb)*cdc

    end do
 
    ind = 0   ! indicator for whether sumwgt = 0

    ! local linear weighted average of CD expenditures for control
    den = G*GPa2*GPb2+2*GPa*GPaPb*GPb-G*(GPaPb)**2-GPa2*(GPb)**2-GPb2*(GPa)**2
    num = GPa2*GPb2*GY-GY*(GPaPb)**2-GPa*GPb2*GPaY+GPb*GPaPb*GPaY+GPa*GPaPb*GPbY-GPb*GPa2*GPbY
    if ((den*num).gt.0) then
       kwgctcdb = num/den
    else if (num.eq.0) then
       kwgctcdb = 0
    else
       ind = 1
    end if

    ! rewind if it's the end of file
    if (rstat2.eq.-1) then 
       rewind (unit=4)
    end if
    
    ! reset the indicator for the end of file
    rstat2 = 1

end subroutine readctrb


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! reads in the data for a pre-treatment group and compute kernel-weighted mean
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine readtrb

    use expn
    implicit none

    integer :: rstat2,tmp
    real :: wgt,pbc,pac,cdc,videoc,admc,toyc,rdc,sportc
    real :: kernel,tempa,tempb,num,den
    real :: G,GPa,GPb,GPa2,GPb2,GPaPb,GY,GPaY,GPbY
    
    ! initialize value
    G = 0     ! sum of kernel weight
    GPa = 0   ! sum of kernel*(Paj-Pai)
    GPb = 0   ! sum of kernel*(Pbj-Pbi)
    GPa2 = 0  ! sum of kernel*(Paj-Pai)**2
    GPb2 = 0  ! sum of kernel*(Pbj-Pbi)**2
    GPaPb = 0 ! sum of kernel*(Paj-Pai)*(Pbj-Pbi)
    GY = 0    ! sum of kernel*CD
    GPaY = 0  ! sum of kernel*(Paj-Pai)*CD
    GPbY = 0  ! sum of kernel*(Pbj-Pbi)*CD

    do while (rstat2.ne.-1)
       read (unit=3,*,IOSTAT=rstat2) wgt,pbc,pac,cdc
       ! two-dimensional biweight kernel
       tempb = (pbc-psb)/h
       tempa = (pac-psa)/h

       if ((abs(tempa).lt.1).and.(abs(tempb).lt.1)) then
          kernel = ((tempa**2-1)**2)*((tempb**2-1)**2)*225/256
       else
          kernel = 0
       end if

       G = G + kernel
       GPa = GPa + kernel*(pac-psa)
       GPb = GPb + kernel*(pbc-psb)
       GPa2 = GPa2 + kernel*(pac-psa)**2
       GPb2 = GPb2 + kernel*(pbc-psb)**2
       GPaPb = GPaPb + kernel*(pac-psa)*(pbc-psb)
       GY = GY + kernel*cdc
       GPaY = GPaY + kernel*(pac-psa)*cdc
       GPbY = GPbY + kernel*(pbc-psb)*cdc

    end do
 
    ind = 0   ! indicator for whether sumwgt = 0

    ! local linear weighted average of CD expenditures for control
    den = G*GPa2*GPb2+2*GPa*GPaPb*GPb-G*(GPaPb)**2-GPa2*(GPb)**2-GPb2*(GPa)**2
    num = GPa2*GPb2*GY-GY*(GPaPb)**2-GPa*GPb2*GPaY+GPb*GPaPb*GPaY+GPa*GPaPb*GPbY-GPb*GPa2*GPbY
    if ((den*num).gt.0) then
       kwgtrcdb = num/den
    else if (num.eq.0) then
       kwgtrcdb = 0
    else
       ind = 1
    end if

    ! rewind if it's the end of file
    if (rstat2.eq.-1) then 
       rewind (unit=3)
    end if
    
    ! reset the indicator for the end of file
    rstat2 = 1

end subroutine readtrb
