program avgDOSCAR

implicit none

integer, parameter :: natommax=400
integer, parameter :: nmaxspec=5

integer i, j, k, l, nspec, unitnum
character(len=50) :: filename
character(len=20) :: dummy(5)
character(len=1)  :: maxl
real :: eshift
integer :: thisnedos
real, allocatable :: eval(:)
logical :: spinpol
integer :: nedos

type spin
    real, allocatable :: bandno(:)
end type spin

type orbital
    type(spin) up,down
end type orbital

type atom
   character(len=2) name
   type(orbital) s(1),p(3),d(5),f(7)
   type(orbital) stot, ptot, dtot, ftot
end type atom

type species
    integer :: numatoms
    type(atom) :: atomno(natommax)
    type(orbital) stot, ptot, dtot, ftot
end type species

type sumdos
    type(spin) up,down
end type sumdos

type(sumdos) summeddos
type(species) spec(nmaxspec)

interface
    subroutine writedos(filename, eval, sup, sdown, pup, pdown, dup, ddown, fup, fdown)
        character(len=*) :: filename
        real, dimension(:) :: eval, sup, sdown, pup, pdown, dup, ddown, fup, fdown
    end subroutine writedos
end interface
    

interface
    subroutine writedosnospin(filename, eval, sup, pup, dup, fup)
        character(len=*) :: filename
        real, dimension(:) :: eval, sup, pup, dup, fup
    end subroutine writedosnospin
end interface
!**************************************************

! Read from the command line the filename, number of species and the number of
! atoms of each species


write(6,*) "input DOS filename:"
read(5,*) filename
write(6,*) "input NEDOS:"
read(5,*) nedos

allocate(eval(nedos))

write(6,*) "input number of species:"
read(5,*) nspec

if (nspec > nmaxspec) then
    write(6,*) "nspec > nmaxspec"
    stop
endif

do i=1,nspec
    write(6,"(A,I2,A)") "number of atoms of species ",i,":"
    read(5,*) spec(i)%numatoms
    if (spec(i)%numatoms > natommax) then
        write(6,*) "spec(i)%numatoms > natommax"
        stop
    endif
enddo

write(6,*) "spin polarised calculation? (T/F):"
read(5,*) spinpol
write(6,*)"spinpol: ",spinpol

write(6,*) "max angular momentum: (d/f):"
read(5,*) maxl

if ((maxl /= "d").and.(maxl /= "f")) then
    write(6,*)"maxl = (d|f)"
    stop
endif

! read header of DOS file
write(6,*)"opening DOS file"
open(unit=10, file=filename, status="old", action="read")

write(6,*)"reading DOS file"

do i=1, 5
    read(10,*) 
enddo

read(10,*) dummy(1), dummy(1), thisnedos, eshift, dummy(1)

! check nedos is correct
! this should be changed to a more elegant solution

if (thisnedos /= nedos) then
    write(6,*) "nedos in file /= nedos in program",thisnedos,nedos
    stop
endif

! read in summed DOS

allocate(summeddos%up%bandno(nedos))
allocate(summeddos%down%bandno(nedos))

if (spinpol) then
    do i=1, nedos
        read(10,*) eval(i), summeddos%up%bandno(i), summeddos%down%bandno(i), dummy(1), dummy(1)
    enddo
else
    do i=1, nedos
        read(10,*) eval(i), summeddos%up%bandno(i), dummy(1)
        write(6,*) eval(i), summeddos%up%bandno(i), dummy(1)
    enddo
endif

write(6,*) eval(nedos),summeddos%up%bandno(nedos)

do i=1, natommax
    do j=1, nspec
        k=1
        allocate(spec(j)%atomno(i)%s(k)%up%bandno(nedos))
        allocate(spec(j)%atomno(i)%s(k)%down%bandno(nedos))
        do k=1, 3
            allocate(spec(j)%atomno(i)%p(k)%up%bandno(nedos))
            allocate(spec(j)%atomno(i)%p(k)%down%bandno(nedos))
        enddo
        do k=1, 5
            allocate(spec(j)%atomno(i)%d(k)%up%bandno(nedos))
            allocate(spec(j)%atomno(i)%d(k)%down%bandno(nedos))
        enddo
        do k=1, 7
            allocate(spec(j)%atomno(i)%f(k)%up%bandno(nedos))
            allocate(spec(j)%atomno(i)%f(k)%down%bandno(nedos))
        enddo
    enddo
enddo

! incase maxl == "d"
do i=1,natommax
    do j=1,nspec
        do k=1, 7
            do l=1,nedos
                spec(j)%atomno(i)%f(k)%up%bandno(l)=0
                spec(j)%atomno(i)%f(k)%down%bandno(l)=0
            enddo
        enddo
    enddo
enddo

write(6,*)" reading atom and angular momenta decomposed dos values"
! read in atom and angular momenta decomposed dos values
if (spinpol) then
    if (maxl=="f") then
        write(6,*)"spin-polarised / s,p,d,f" 
        do i=1,nspec
            do j=1,spec(i)%numatoms
                read(10,*) dummy

                do k=1,nedos
                    read(10,*) dummy(1), &
                               spec(i)%atomno(j)%s(1)%up%bandno(k), &
                               spec(i)%atomno(j)%s(1)%down%bandno(k), &

                               spec(i)%atomno(j)%p(1)%up%bandno(k), &
                               spec(i)%atomno(j)%p(1)%down%bandno(k), &
                               spec(i)%atomno(j)%p(2)%up%bandno(k), &
                               spec(i)%atomno(j)%p(2)%down%bandno(k), &
                               spec(i)%atomno(j)%p(3)%up%bandno(k), &
                               spec(i)%atomno(j)%p(3)%down%bandno(k), &

                               spec(i)%atomno(j)%d(1)%up%bandno(k), &
                               spec(i)%atomno(j)%d(1)%down%bandno(k), &
                               spec(i)%atomno(j)%d(2)%up%bandno(k), &
                               spec(i)%atomno(j)%d(2)%down%bandno(k), &
                               spec(i)%atomno(j)%d(3)%up%bandno(k), &
                               spec(i)%atomno(j)%d(3)%down%bandno(k), &
                               spec(i)%atomno(j)%d(4)%up%bandno(k), &
                               spec(i)%atomno(j)%d(4)%down%bandno(k), &
                               spec(i)%atomno(j)%d(5)%up%bandno(k), &
                               spec(i)%atomno(j)%d(5)%down%bandno(k), &

                               spec(i)%atomno(j)%f(1)%up%bandno(k), &
                               spec(i)%atomno(j)%f(1)%down%bandno(k), &
                               spec(i)%atomno(j)%f(2)%up%bandno(k), &
                               spec(i)%atomno(j)%f(2)%down%bandno(k), &
                               spec(i)%atomno(j)%f(3)%up%bandno(k), &
                               spec(i)%atomno(j)%f(3)%down%bandno(k), &
                               spec(i)%atomno(j)%f(4)%up%bandno(k), &
                               spec(i)%atomno(j)%f(4)%down%bandno(k), &
                               spec(i)%atomno(j)%f(5)%up%bandno(k), &
                               spec(i)%atomno(j)%f(5)%down%bandno(k), &
                               spec(i)%atomno(j)%f(6)%up%bandno(k), &
                               spec(i)%atomno(j)%f(6)%down%bandno(k), &
                               spec(i)%atomno(j)%f(7)%up%bandno(k), &
                               spec(i)%atomno(j)%f(7)%down%bandno(k)
                enddo
            enddo
        enddo
    else
        write(6,*)"spin-polarised / s,p,d" 
        do i=1,nspec
            do j=1,spec(i)%numatoms
                read(10,*) dummy

                do k=1,nedos
                    read(10,*) dummy(1), &
                               spec(i)%atomno(j)%s(1)%up%bandno(k), &
                               spec(i)%atomno(j)%s(1)%down%bandno(k), &

                               spec(i)%atomno(j)%p(1)%up%bandno(k), &
                               spec(i)%atomno(j)%p(1)%down%bandno(k), &
                               spec(i)%atomno(j)%p(2)%up%bandno(k), &
                               spec(i)%atomno(j)%p(2)%down%bandno(k), &
                               spec(i)%atomno(j)%p(3)%up%bandno(k), &
                               spec(i)%atomno(j)%p(3)%down%bandno(k), &

                               spec(i)%atomno(j)%d(1)%up%bandno(k), &
                               spec(i)%atomno(j)%d(1)%down%bandno(k), &
                               spec(i)%atomno(j)%d(2)%up%bandno(k), &
                               spec(i)%atomno(j)%d(2)%down%bandno(k), &
                               spec(i)%atomno(j)%d(3)%up%bandno(k), &
                               spec(i)%atomno(j)%d(3)%down%bandno(k), &
                               spec(i)%atomno(j)%d(4)%up%bandno(k), &
                               spec(i)%atomno(j)%d(4)%down%bandno(k), &
                               spec(i)%atomno(j)%d(5)%up%bandno(k), &
                               spec(i)%atomno(j)%d(5)%down%bandno(k)
                enddo
            enddo
        enddo
    endif
else
    if (maxl=="f") then
        write(6,*)"spin-unpolarised / s,p,d,f"
        do i=1,nspec
            do j=1,spec(i)%numatoms
                read(10,*) dummy

                do k=1,nedos
!                   write(6,*) i,j,k
                    read(10,*) dummy(1), &
                               spec(i)%atomno(j)%s(1)%up%bandno(k), &

                               spec(i)%atomno(j)%p(1)%up%bandno(k), &
                               spec(i)%atomno(j)%p(2)%up%bandno(k), &
                               spec(i)%atomno(j)%p(3)%up%bandno(k), &
    
                               spec(i)%atomno(j)%d(1)%up%bandno(k), &
                               spec(i)%atomno(j)%d(2)%up%bandno(k), &
                               spec(i)%atomno(j)%d(3)%up%bandno(k), &
                               spec(i)%atomno(j)%d(4)%up%bandno(k), &
                               spec(i)%atomno(j)%d(5)%up%bandno(k), &

                               spec(i)%atomno(j)%f(1)%up%bandno(k), &
                               spec(i)%atomno(j)%f(2)%up%bandno(k), &
                               spec(i)%atomno(j)%f(3)%up%bandno(k), &
                               spec(i)%atomno(j)%f(4)%up%bandno(k), &
                               spec(i)%atomno(j)%f(5)%up%bandno(k), &
                               spec(i)%atomno(j)%f(6)%up%bandno(k), &
                               spec(i)%atomno(j)%f(7)%up%bandno(k)
                enddo
            enddo
        enddo
    else
        write(6,*)"spin-unpolarised / s,p,d"
        do i=1,nspec
            do j=1,spec(i)%numatoms
                read(10,*) dummy(1),dummy(2),dummy(3),dummy(4),dummy(5)
                write(6,*) dummy(1),dummy(2),dummy(3),dummy(4),dummy(5)
                do k=1,nedos
                    read(10,*) dummy(1), &
                               spec(i)%atomno(j)%s(1)%up%bandno(k), &

                               spec(i)%atomno(j)%p(1)%up%bandno(k), &
                               spec(i)%atomno(j)%p(2)%up%bandno(k), &
                               spec(i)%atomno(j)%p(3)%up%bandno(k), &
    
                               spec(i)%atomno(j)%d(1)%up%bandno(k), &
                               spec(i)%atomno(j)%d(2)%up%bandno(k), &
                               spec(i)%atomno(j)%d(3)%up%bandno(k), &
                               spec(i)%atomno(j)%d(4)%up%bandno(k), &
                               spec(i)%atomno(j)%d(5)%up%bandno(k)
                enddo
            enddo
        enddo
    endif
endif


if (spinpol) then
! multiply all down spin values by -1

    do i=1, nspec
        do j=1, spec(i)%numatoms
            do k=1,nedos
    
                spec(i)%atomno(j)%s(1)%down%bandno(k) = &
                -spec(i)%atomno(j)%s(1)%down%bandno(k)

                spec(i)%atomno(j)%p(1)%down%bandno(k) = &
                -spec(i)%atomno(j)%p(1)%down%bandno(k)

                spec(i)%atomno(j)%p(2)%down%bandno(k) = &
                -spec(i)%atomno(j)%p(2)%down%bandno(k)

                spec(i)%atomno(j)%p(3)%down%bandno(k) = &
                -spec(i)%atomno(j)%p(3)%down%bandno(k)

                spec(i)%atomno(j)%d(1)%down%bandno(k) = &
                -spec(i)%atomno(j)%d(1)%down%bandno(k)

                spec(i)%atomno(j)%d(2)%down%bandno(k) = &
                -spec(i)%atomno(j)%d(2)%down%bandno(k)

                spec(i)%atomno(j)%d(3)%down%bandno(k) = &
                -spec(i)%atomno(j)%d(3)%down%bandno(k)

                spec(i)%atomno(j)%d(4)%down%bandno(k) = &
                -spec(i)%atomno(j)%d(4)%down%bandno(k)

                spec(i)%atomno(j)%d(5)%down%bandno(k) = &
                -spec(i)%atomno(j)%d(5)%down%bandno(k)
            enddo
        enddo
    enddo

    if (maxl=="f") then
        do i=1, nspec
            do j=1, spec(i)%numatoms
                do k=1,nedos
                    spec(i)%atomno(j)%f(1)%down%bandno(k) = &
                    -spec(i)%atomno(j)%f(1)%down%bandno(k)

                    spec(i)%atomno(j)%f(2)%down%bandno(k) = &
                    -spec(i)%atomno(j)%f(2)%down%bandno(k)

                    spec(i)%atomno(j)%f(3)%down%bandno(k) = &
                    -spec(i)%atomno(j)%f(3)%down%bandno(k)

                    spec(i)%atomno(j)%f(4)%down%bandno(k) = &
                    -spec(i)%atomno(j)%f(4)%down%bandno(k)

                    spec(i)%atomno(j)%f(5)%down%bandno(k) = &
                    -spec(i)%atomno(j)%f(5)%down%bandno(k)

                    spec(i)%atomno(j)%f(6)%down%bandno(k) = &
                    -spec(i)%atomno(j)%f(6)%down%bandno(k)

                    spec(i)%atomno(j)%f(7)%down%bandno(k) = &
                    -spec(i)%atomno(j)%f(7)%down%bandno(k)
                enddo
            enddo
        enddo
    endif
endif

! average over angular momenta for each atom
do i=1, nspec
    do j=1, spec(i)%numatoms
        allocate(spec(i)%atomno(j)%stot%up%bandno(nedos))
        allocate(spec(i)%atomno(j)%stot%down%bandno(nedos))
        allocate(spec(i)%atomno(j)%ptot%up%bandno(nedos))
        allocate(spec(i)%atomno(j)%ptot%down%bandno(nedos))
        allocate(spec(i)%atomno(j)%dtot%up%bandno(nedos))
        allocate(spec(i)%atomno(j)%dtot%down%bandno(nedos))
        allocate(spec(i)%atomno(j)%ftot%up%bandno(nedos))
        allocate(spec(i)%atomno(j)%ftot%down%bandno(nedos))


        spec(i)%atomno(j)%stot%up%bandno(:) = 0
        spec(i)%atomno(j)%ptot%up%bandno(:) = 0
        spec(i)%atomno(j)%dtot%up%bandno(:) = 0
        spec(i)%atomno(j)%ftot%up%bandno(:) = 0
        
        do k=1, nedos
            l=1
            
            spec(i)%atomno(j)%stot%up%bandno(k) = &
            spec(i)%atomno(j)%stot%up%bandno(k) + &
            spec(i)%atomno(j)%s(l)%up%bandno(k)

            do l=1,3
                spec(i)%atomno(j)%ptot%up%bandno(k) = &
                spec(i)%atomno(j)%ptot%up%bandno(k) + &
                spec(i)%atomno(j)%p(l)%up%bandno(k)
            enddo

            do l=1,5
                spec(i)%atomno(j)%dtot%up%bandno(k) = &
                spec(i)%atomno(j)%dtot%up%bandno(k) + &
                spec(i)%atomno(j)%d(l)%up%bandno(k)
            enddo
        
            if(maxl=="f")then
                do l=1,7
                    spec(i)%atomno(j)%ftot%up%bandno(k) = &
                    spec(i)%atomno(j)%ftot%up%bandno(k) + &
                    spec(i)%atomno(j)%f(l)%up%bandno(k)
                enddo
            endif
        enddo
    enddo
enddo

if (spinpol) then
    do i=1, nspec
        do j=1, spec(i)%numatoms

            spec(i)%atomno(j)%stot%down%bandno(:) = 0
            spec(i)%atomno(j)%ptot%down%bandno(:) = 0
            spec(i)%atomno(j)%dtot%down%bandno(:) = 0
            spec(i)%atomno(j)%ftot%down%bandno(:) = 0
        
            do k=1, nedos
                l=1
            
                spec(i)%atomno(j)%stot%down%bandno(k) = &
                spec(i)%atomno(j)%stot%down%bandno(k) + &
                spec(i)%atomno(j)%s(l)%down%bandno(k)

                do l=1,3
                    spec(i)%atomno(j)%ptot%down%bandno(k) = &
                    spec(i)%atomno(j)%ptot%down%bandno(k) + &
                    spec(i)%atomno(j)%p(l)%down%bandno(k)
                enddo

                do l=1,5
                    spec(i)%atomno(j)%dtot%down%bandno(k) = &
                    spec(i)%atomno(j)%dtot%down%bandno(k) + &
                    spec(i)%atomno(j)%d(l)%down%bandno(k)
                enddo
        
                if (maxl == "f") then
                    do l=1,7
                        spec(i)%atomno(j)%ftot%down%bandno(k) = &
                        spec(i)%atomno(j)%ftot%down%bandno(k) + &
                        spec(i)%atomno(j)%f(l)%down%bandno(k)
                    enddo
                endif
            enddo
        enddo
    enddo
endif
        
! average over angular momenta for each species

do i=1, nspec
    allocate(spec(i)%stot%up%bandno(nedos))
    allocate(spec(i)%stot%down%bandno(nedos))
    allocate(spec(i)%ptot%up%bandno(nedos))
    allocate(spec(i)%ptot%down%bandno(nedos))
    allocate(spec(i)%dtot%up%bandno(nedos))
    allocate(spec(i)%dtot%down%bandno(nedos))
    allocate(spec(i)%ftot%up%bandno(nedos))
    allocate(spec(i)%ftot%down%bandno(nedos))

    spec(i)%stot%up%bandno(:) = 0
    spec(i)%stot%down%bandno(:) = 0
    spec(i)%ptot%up%bandno(:) = 0
    spec(i)%ptot%down%bandno(:) = 0
    spec(i)%dtot%up%bandno(:) = 0
    spec(i)%dtot%down%bandno(:) = 0
    spec(i)%ftot%up%bandno(:) = 0
    spec(i)%ftot%down%bandno(:) = 0

    do j=1, spec(i)%numatoms
        do k=1, nedos
            spec(i)%stot%up%bandno(k) = spec(i)%stot%up%bandno(k) + &
            spec(i)%atomno(j)%stot%up%bandno(k)
            
            spec(i)%ptot%up%bandno(k) = spec(i)%ptot%up%bandno(k) + &
            spec(i)%atomno(j)%ptot%up%bandno(k)
            
            spec(i)%dtot%up%bandno(k) = spec(i)%dtot%up%bandno(k) + &
            spec(i)%atomno(j)%dtot%up%bandno(k)
            
            if (maxl=="f") then
                spec(i)%ftot%up%bandno(k) = spec(i)%ftot%up%bandno(k) + &
                spec(i)%atomno(j)%ftot%up%bandno(k)
            endif
        enddo
    enddo
enddo

if (spinpol) then
    do i=1, nspec

        spec(i)%stot%down%bandno(:) = 0
        spec(i)%ptot%down%bandno(:) = 0
        spec(i)%dtot%down%bandno(:) = 0
        spec(i)%ftot%down%bandno(:) = 0

        do j=1, spec(i)%numatoms
            do k=1, nedos
                spec(i)%stot%down%bandno(k) = spec(i)%stot%down%bandno(k) + &
                spec(i)%atomno(j)%stot%down%bandno(k)
            
                spec(i)%ptot%down%bandno(k) = spec(i)%ptot%down%bandno(k) + &
                spec(i)%atomno(j)%ptot%down%bandno(k)
            
                spec(i)%dtot%down%bandno(k) = spec(i)%dtot%down%bandno(k) + &
                spec(i)%atomno(j)%dtot%down%bandno(k)
            
                if (maxl == "f") then
                    spec(i)%ftot%down%bandno(k) = spec(i)%ftot%down%bandno(k) + &
                    spec(i)%atomno(j)%ftot%down%bandno(k)
                endif
            enddo
        enddo
    enddo
endif

! shift energy values to give correct Fermi level
do i=1, nedos
    eval(i) = eval(i) - eshift
enddo

! write angular momenta and species decomposed DOS

if (spinpol) then
    do i=1, nspec
        call writedos("fort."//char(48+i), eval(1:nedos), &
                              spec(i)%stot%up%bandno(1:nedos), &
                              spec(i)%stot%down%bandno(1:nedos), &
                              spec(i)%ptot%up%bandno(1:nedos), &
                              spec(i)%ptot%down%bandno(1:nedos), &
                              spec(i)%dtot%up%bandno(1:nedos), &
                              spec(i)%dtot%down%bandno(1:nedos), &
                              spec(i)%ftot%up%bandno(1:nedos), &
                              spec(i)%ftot%down%bandno(1:nedos))

    enddo
else
    do i=1, nspec
        call writedosnospin("fort."//char(48+i), eval(1:nedos), &
                                    spec(i)%stot%up%bandno(1:nedos), &
                                    spec(i)%ptot%up%bandno(1:nedos), &
                                    spec(i)%dtot%up%bandno(1:nedos), &
                                    spec(i)%ftot%up%bandno(1:nedos))

    enddo
endif

end

subroutine writedos(filename, eval, sup, sdown, pup, pdown, dup, ddown, fup, fdown)

character(len=*) :: filename
real, dimension(:) :: eval, sup, sdown, pup, pdown, dup, ddown, fup, fdown

open(unit=11, file=filename, status="new", action="write")

write(11,*)"# Energy/eV s(up) s(down) p(up) p(down) d(up) d(down) f(up) f(down)"

do j=1, size(eval)
    write(11,'(f12.6,8(x,e10.4))') eval(j), sup(j), sdown(j), &
                                            pup(j), pdown(j), &
                                            dup(j), ddown(j), &
                                            fup(j), fdown(j)
enddo

close(11)

end subroutine writedos

subroutine writedosnospin(filename, eval, sup, pup, dup, fup)

character(len=*) :: filename
real, dimension(:) :: eval, sup, pup, dup, fup

open(unit=11, file=filename, status="new", action="write")

write(11,*)"Energy/eV s p d f"

do j=1, size(eval)
    write(11,'(f12.6,4(x,e10.4))') eval(j), sup(j), &
                                            pup(j), &
                                            dup(j), &
                                            fup(j)
enddo

close(11)

end subroutine writedosnospin

