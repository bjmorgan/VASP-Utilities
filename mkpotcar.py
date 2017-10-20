#! /usr/bin/env python3

import os
import argparse
import sys

parser = argparse.ArgumentParser( description='Construct a VASP POTCAR file' )
parser.add_argument( 'potcars', metavar='P', type=str, nargs='+', help='the string identifying each POTCAR directory.' )
parser.add_argument( '--potcar-set', default='PBE_54', help='choose which POTCAR set to use' )
args = parser.parse_args()

try:
    potcardir = os.path.join( os.environ['POTCARDIR'], args.potcar_set )
except KeyError:
    raise( '$POTCARDIR is not set' )

if not os.path.isdir( potcardir ):
    raise ValueError( 'Cannot find directory f{potcardir}' )

for potcar in args.potcars:
    if not os.path.isdir( os.path.join( potcardir, potcar ) ):
        print( '{} not found in {}'.format( potcar, potcardir ) )
        print( 'Available pseudopotentials ({})'.format( args.potcar_set) )
        os.system( 'get_potcar_list.py' ) # This should be a module call instead.
        sys.exit()

for potcar in args.potcars:
    os.system( 'cat {}/POTCAR'.format( os.path.join( potcardir, potcar ) ) )
