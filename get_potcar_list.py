#! /usr/bin/env python3

import numpy as np
import argparse
import os

parser = argparse.ArgumentParser()
parser.add_argument( '-l', '--list', help='Print the list of pseudopotentials as a formatted list.', action='store_true' )
parser.add_argument( '-c', '--columns', help='Set the number of columns for list output', type=int, default=0 )
args = parser.parse_args()

if 'POTCARDIR' not in os.environ:
    raise AttributeError( 'The POTCARDIR environment variable is not set.' )
potcar_dir = os.environ['POTCARDIR']

if not os.path.isdir( potcar_dir ):
    raise ValueError( 'The POTCARDIR environment variable does not give a valid directory: {}'.format( potcar_dir ) )

dir_name = potcar_dir.split('/')[-1]

potcars = [ w[0].split('/')[-1] for w in os.walk( potcar_dir ) ] 
potcars.remove( dir_name )
potcars.sort()

if args.list:
    if args.columns == 0:
        # check the width of the current window
        rows, columns = os.popen('stty size', 'r').read().split()
        n_columns = int( columns ) // 10 - 1
    else:
        n_columns = args.columns    
    to_pad = n_columns - len( potcars ) % n_columns 
    n = np.array( potcars + [''] * to_pad ).reshape(n_columns,-1).T
    for i in range( 0, len( potcars ), n_columns ):
        print( ' '.join(('%*s' % (10, pp) for pp in potcars[i:i+n_columns])) )
else:
    print( ' '.join( potcars ) )

