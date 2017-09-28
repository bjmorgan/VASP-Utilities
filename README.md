# VASP Utilities

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

## checkforce

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
    
## checkmag

usage: checkmag.py [-h] [-o OUTCAR] [-t THRESHOLD]

optional arguments:
  -h, --help            show this help message and exit
  -o OUTCAR, --outcar OUTCAR
                        OUTCAR file to read from
  -t THRESHOLD, --threshold THRESHOLD
                        only report magnetic moments larger than this
                        threshold.
	
Python dependencies: `pymatgen`
	
## checkmurn

Checks convergence for calculations running for a Murnaghan equation of state fit. Each subdirectory that begins with a numeral is treated as a lattice parameter label, and [checkforce](#checkforce) is called.

## lspotcar

Requires $POTCARDIR set as an environment variable.
Acts as an alias for ls $POTCARDIR so that the environment variable is hidden.
Required by [mkpotcar](#mkpotcar)

## mkmurn

Identifies directories that match `./[0-9]*` and uses the cell scaling in POTCAR, and final energy in OUTCAR from each to construct an input file for `murn`.

## mkpotcar

Requires $POTCARDIR set as an environment variable.
Syntax is `mkpotcar <P1> <P2> ...`, where `P1` and `P2` (etc.) are names of pseudo potential directories in $POTCARDIR.
If any of the requested directories are not found in $POTCARDIR then [lspotcar](#lspotcar) is called to give a listing of the available pseudo potentials.

## mktrajectory

Generates a `.xyz` formatted trajectory file from the sequence of ionic positions saved in OUTCAR. Requires POSCAR to read in the numbers of each ionic species.

## murncalc

Calls [mkmurn](#mkmurn) to generate a Murnaghan fit input file, pipes this to `murn`, and identifies the zero pressure lattice parameter from the output.

## parseDOSCARf

`parsedoscar` is now a separate repository at https://github.com/bjmorgan/parsedoscar.

## README.md

This file.
