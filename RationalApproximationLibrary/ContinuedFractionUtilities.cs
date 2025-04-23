using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;
using System.Text;
using System.Threading.Tasks;

namespace RationalApproximationLibrary
{
    public static class ContinuedFractionUtilities
    {
        public static IEnumerable<BigInteger> EnumerateContinuedFraction( BigInteger n, BigInteger d )
        {
            if( n < 0 ) throw new ArgumentException( "Negative nominator", nameof( n ) );
            //if( d.IsZero ) throw new ArgumentException( "Zero denominator", nameof( d ) );

            for(; ; )
            {
                (BigInteger q, BigInteger r) = BigInteger.DivRem( n, d );

                yield return q;

                if( r.IsZero ) break;

                n = d;
                d = r;
            }
        }

        public static IEnumerable<(BigInteger n, BigInteger d)> EnumerateContinuedFractionConvergents( IReadOnlyList<BigInteger> continuedFraction )
        {
            // https://r-knott.surrey.ac.uk/Fibonacci/cfINTRO.html#convergrecurr

            if( continuedFraction.Count <= 0 ) throw new ArgumentException( "Empty continued fraction", nameof( continuedFraction ) );

            BigInteger prev_n = BigInteger.One;
            BigInteger prev_d = BigInteger.Zero;
            BigInteger n = continuedFraction[0];
            BigInteger d = BigInteger.One;

            yield return (n, d);

            for( int i = 1; i < continuedFraction.Count; i++ )
            {
                BigInteger new_n = continuedFraction[i] * n + prev_n;
                BigInteger new_d = continuedFraction[i] * d + prev_d;

                //Debug.WriteLine( $"{new_n:D}/{new_d:D}" );

                prev_n = n;
                prev_d = d;
                n = new_n;
                d = new_d;

                yield return (n, d);
            }
        }

        public static BigInteger[] Negate( BigInteger[] regularContinuedFraction )
        {
            // https://pi.math.cornell.edu/~gautam/ContinuedFractions.pdf
            // https://kconrad.math.uconn.edu/blurbs/ugradnumthy/contfrac-neg-invert.pdf

            BigInteger[] result;

            BigInteger new_a0 = -regularContinuedFraction[0] - 1;

            switch( regularContinuedFraction.Length )
            {
            case 0:
            default:
                throw new InvalidOperationException( );
            case 1:
                result = [-regularContinuedFraction[0]];
                break;
            case 2:
            {
                BigInteger a1 = regularContinuedFraction[1];

                if( a1 != 1 )
                {
                    Debug.Assert( a1 > 1 );

                    result = [new_a0, 1, a1 - 1];
                }
                else
                {
                    // normally this should not be achieved,
                    // since [a0; 1] is a0+1, i.e. [a0+1],
                    // and the negation is [-a0-1]

                    result = [new_a0];
                }
            }
            break;
            case > 2:
            {
                BigInteger a1 = regularContinuedFraction[1];

                if( a1 != 1 )
                {
                    Debug.Assert( a1 > 1 );

                    result = [new_a0, 1, a1 - 1, .. regularContinuedFraction[2..]];
                }
                else
                {
                    BigInteger a2 = regularContinuedFraction[2];

                    result = [new_a0, a2 + 1, .. regularContinuedFraction[3..]];
                }
            }
            break;
            }

            return result;
        }
    }
}
