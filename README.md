#VASP Utilities

2011-11-14_README

Shell scripts and other code for helping with processing VASP input / output.

Repository contents:

* [checkforce] (#checkforce)
* [checkmag] (#checkmag)
* [checkmurn] (#checkmurn)
* [lspotcar] (#lspotcar)
* [mkmurn] (#mkmurn)
* [mkpotcar] (#mkpotcar)
* [mktrajectory] (#mktrajectory)
* [murncalc] (#murncalc)
* [parseDOSCARf.f90] (#parseDOSCARf)
* [README] (#readme)

##checkforce

Looks for OUTCAR in the current directory, and checks the most recently outputted set of forces against the convergence criterion.

For options use `checkforce -h`:

    List force convergence from VASP OUTCAR

    Optional arguments:
      -h, --help                  print this message

      -c, --convergence=CONV      set force convergence to CONV [this will override --outcar]
      -o, --outcar                read force convergence from OUTCAR 
      -v, --verbose               show convergence status for all atoms

    Standard output:
    remainder: < sum{force on atom - force convergence} / number of atoms >
    maximum: < max non-converged forces >
    not-opt: < number of non-converged forces > / < number of atoms >
    
##checkmag

Outputs magnetization data from an OUTCAR file (default is to use current directory)

For options use `checkmag -g`:

	Outputs ion magnetization data from a VASP OUTCAR file
  
	  Usage: checkmag [options] outcar_name

	    -m, --mag NUM                    Set magnetization cutoff (default = 0.5)
	    -v, --verbose                    Output all ions
	    -s, --steps                      Output data for every step
	    -h, --help                       Display this screen
		
##checkmurn

Checks convergence for calculations running for a Murnaghan equation of state fit. Each subdirectory that begins with a numeral is treated as a lattice parameter label, and [checkforce](#checkforce) is called.

##lspotcar

Requires $POTCARDIR set as an environment variable.
Acts as an alias for ls $POTCARDIR so that the environment variable is hidden.
Required by [mkpotcar](#mkpotcar)

##mkmurn

Identifies directories that match `./[0-9]*` and uses the cell scaling in POTCAR, and final energy in OUTCAR from each to construct an input file for `murn`.

##mkpotcar

Requires $POTCARDIR set as an environment variable.
Syntax is `mkpotcar \<P1\> \<P2\> ...`, where P1 and P2 (etc.) are names of pseudo potential directories in $POTCARDIR.
If any of the requested directories are not found in $POTCARDIR then [lspotcar](#lspotcar) is called to give a listing of the available pseudo potentials.

##mktrajectory

Generates a `.xyz` formatted trajectory file from the sequence of ionic positions saved in OUTCAR. Requires POSCAR to read in the numbers of each ionic species.

##murncalc

Calls [mkmurn](#mkmurn) to generate a Murnaghan fit input file, pipes this to `murn`, and identifies the zero pressure lattice parameter from the output.

##parseDOSCARf

Fortran90 code to convert from DOSCAR to {s, p, d, f} projected densities of states as nxy format.

Reads input parameters from `parsedoscar.inpt`, which looks like:

    DOSCAR     Filename to read from
    1600              NEDOS
    4                    Number of atomic species (maximum is currently 5: see nmaxspec in the source code.)
    1                    Number of atoms of species 1
    1                    Number of atoms of species 2
    35                  Number of atoms of species 3
    72                  Number of atoms of species 4 ...
    .true.              Spin polarised calculation? ( .true. / .false. )
    d                    Maximum angular momentum ( d / f )

## read me

This file.