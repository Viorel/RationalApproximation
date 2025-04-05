using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RationalApproximationLibrary;

namespace RationalApproximationLibrary
{
    internal static class DFloatUtilities
    {
        public static DFloat GreatestCommonDivisor( ICancellable cnc, DFloat a, DFloat b, int maxBytes )
        {
            // assuming that 'a' and 'b' are integers
            // TODO: check
            // TODO: use 'BigInteger.GreatestCommonDivisor' for appropriate 'a' and 'b'

            Debug.Assert( a.IsPositiveOrZero );
            Debug.Assert( b.IsPositiveOrZero );

            for(; ; )
            {
                cnc.TryThrow( );

                if( b.IsZero ) return a.UnionApprox( b );

                (a, b) = (b, DFloat.Remainder( a, b, maxBytes ));
            }
        }

        public static DFloat LeastCommonMultiple( ICancellable cnc, DFloat a, DFloat b, int maxBytes )
        {
            // assuming that 'a' and 'b' are integers
            // TODO: check

            Debug.Assert( a.IsPositiveOrZero );
            Debug.Assert( b.IsPositiveOrZero );

            if( a.IsZero && b.IsZero ) return DFloat.Zero;

            int mb = maxBytes + 4;

            DFloat gcd = GreatestCommonDivisor( cnc, a, b, mb );

            return DFloat.Mul( DFloat.Div( b, gcd, mb ), a, maxBytes );
        }
    }
}
