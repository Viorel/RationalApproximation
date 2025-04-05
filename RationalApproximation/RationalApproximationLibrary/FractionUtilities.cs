using System.Diagnostics;
using System.Numerics;

namespace RationalApproximationLibrary
{
    // NOTE. Unused code is almost deleted.

    internal static class FractionUtilities
    {
        static readonly BigInteger Bi10 = 10;
        static readonly BigInteger Bi100 = 100;

        public static BigInteger GreatestCommonDivisor( ICancellable cnc, BigInteger a, BigInteger b )
        {
            ArgumentOutOfRangeException.ThrowIfNegative( a );
            ArgumentOutOfRangeException.ThrowIfNegative( b );

            if( a < ulong.MaxValue && b < ulong.MaxValue ) // TODO: investigate if this condition is suitable
            {
                return BigInteger.GreatestCommonDivisor( a, b );
            }
            else
            {
                return GreatestCommonDivisorEuclidean( cnc, a, b );
            }
        }

        public static BigInteger GreatestCommonDivisorEuclidean( ICancellable cnc, BigInteger a, BigInteger b )
        {
            ArgumentOutOfRangeException.ThrowIfNegative( a );
            ArgumentOutOfRangeException.ThrowIfNegative( b );

            for(; ; )
            {
                cnc.TryThrow( );

                if( b.IsZero ) return a;

                (a, b) = (b, a % b);
            }
        }

        public static BigInteger LeastCommonMultiple( ICancellable cnc, BigInteger a, BigInteger b )
        {
            ArgumentOutOfRangeException.ThrowIfNegative( a );
            ArgumentOutOfRangeException.ThrowIfNegative( b );

            if( a.IsZero && b.IsZero ) return BigInteger.Zero;

            BigInteger gcd = GreatestCommonDivisor( cnc, a, b );

            return b / gcd * a;
        }

        public static (BigInteger x, int e) TrimZeroes( ICancellable cnc, BigInteger x )
        {
            (BigInteger new_x, int new_e) = TrimZeroesGE0( cnc, x < 0 ? -x : x );

            return (x < 0 ? -new_x : new_x, new_e);
        }

        public static (BigInteger x, int e) TrimZeroesGE0( ICancellable cnc, BigInteger x )
        {
            Debug.Assert( x >= 0 );

            int e = 0;

            if( x >= Bi100 )
            {
                for(; ; )
                {
                    cnc.TryThrow( );

                    (BigInteger q, BigInteger r) = BigInteger.DivRem( x, Bi100 );

                    if( !r.IsZero ) break;

                    x = q;
                    e += 2;
                }
            }

            if( x >= Bi10 )
            {
                for(; ; )
                {
                    cnc.TryThrow( );

                    (BigInteger q, BigInteger r) = BigInteger.DivRem( x, Bi10 );

                    if( !r.IsZero ) break;

                    x = q;
                    ++e;
                }
            }

            return (x, e);
        }

        internal static int NumberOfDigits( ICancellable cnc, BigInteger x )
        {
            return NumberOfDigitsGE0( cnc, x < 0 ? -x : x );
        }

        internal static int NumberOfDigitsGE0( ICancellable cnc, BigInteger x )
        {
            ArgumentOutOfRangeException.ThrowIfNegative( x );

            int r = 1;

            for(; ; )
            {
                x = BigInteger.Divide( x, Bi10 );
                if( x.IsZero ) break;
                ++r;

                cnc.TryThrow( );
            }

            return r;
        }

        internal static (BigInteger n, BigInteger d) NDEtoND( (BigInteger n, BigInteger d, BigInteger e) f )
        {
            Debug.Assert( f.e >= -1000 && f.e <= 1000 );

            var t =
                f.e == 0 ? (f.n, f.d)
                : f.e > 0 ? (f.n * BigInteger.Pow( Bi10, checked((int)f.e) ), f.d)
                : (f.n, f.d * BigInteger.Pow( Bi10, -checked((int)f.e) ));

            return t;
        }

        internal static int Compare( (BigInteger n, BigInteger d, BigInteger e) f1, (BigInteger n, BigInteger d, BigInteger e) f2 )
        {
            Debug.Assert( f1.d > 0 );
            Debug.Assert( f2.d > 0 );

            if( f1.n.IsZero ) return -f2.n.Sign;
            if( f2.n.IsZero ) return f1.n.Sign;
            if( f1.n < 0 && f2.n > 0 ) return -1;
            if( f1.n > 0 && f2.n < 0 ) return +1;

            BigInteger p1 = f1.n * f2.d;
            BigInteger e1 = f1.e;
            BigInteger p2 = f2.n * f1.d;
            BigInteger e2 = f2.e;

            if( p1 == p2 ) return p1 < 0 ? -e1.CompareTo( e2 ) : e1.CompareTo( e2 );

            Debug.Assert( p1.Sign == p2.Sign );
            Debug.Assert( p1 != 0 && p2 != 0 );

            if( e1 < e2 )
            {
                do
                {
                    var (q, r) = BigInteger.DivRem( p1, Bi10 );
                    if( !r.IsZero ) break;

                    ++e1;
                    p1 = q;

                } while( e1 < e2 );
            }
            else if( e1 > e2 )
            {
                do
                {
                    var (q, r) = BigInteger.DivRem( p2, Bi10 );
                    if( !r.IsZero ) break;

                    ++e2;
                    p2 = q;

                } while( e2 < e1 );
            }

            if( e1 == e2 ) return p1.CompareTo( p2 );
            if( p1 == p2 ) return p1 < 0 ? -e1.CompareTo( e2 ) : e1.CompareTo( e2 );

            Debug.Assert( p1.Sign == p2.Sign );
            Debug.Assert( p1 != 0 && p2 != 0 );

            int sign;

            if( p1 < 0 )
            {
                p1 = -p1;
                p2 = -p2;
                sign = -1;
            }
            else
            {
                sign = +1;
            }

            if( e1 > e2 )
            {
                do
                {
                    if( p1 >= p2 ) return sign;

                    p1 *= Bi10;
                    --e1;
                } while( e1 > e2 );
            }
            else
            {
                Debug.Assert( e1 < e2 );

                do
                {
                    if( p1 <= p2 ) return -sign;

                    p2 *= Bi10;
                    --e2;
                } while( e1 < e2 );
            }

            Debug.Assert( e1 == e2 );

            return sign < 0 ? -p1.CompareTo( p2 ) : p1.CompareTo( p2 );
        }

        /// <summary>
        /// Difference of two fractions, when the difference of "E" is not too large.
        /// </summary>
        /// <param name="f1">The first fraction</param>
        /// <param name="f2">The second fraction</param>
        /// <returns>The difference</returns>
        internal static (BigInteger n, BigInteger d, BigInteger e) DiffSmallDiffE( (BigInteger n, BigInteger d, BigInteger e) f1, (BigInteger n, BigInteger d, BigInteger e) f2 )
        {
            var min_e = BigInteger.Min( f1.e, f2.e );

            var f1x = NDEtoND( (f1.n, f1.d, f1.e - min_e) );
            var f2x = NDEtoND( (f2.n, f2.d, f2.e - min_e) );

            return (f1x.n * f2x.d - f1x.d * f2x.n, f1x.d * f2x.d, min_e);
        }

        internal static (BigInteger n, BigInteger d, BigInteger e) Abs( (BigInteger n, BigInteger d, BigInteger e) f )
        {
            return f.n < 0 ? (-f.n, f.d, f.e) : f;
        }

    }
}
