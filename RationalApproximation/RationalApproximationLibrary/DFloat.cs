using System;
using System.CodeDom;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;
using System.Text;
using System.Threading.Tasks;


namespace RationalApproximationLibrary
{
    // NOTE. Unused code is almost deleted.

    /// <summary>
    /// A floating point number defined as <i>“m * 10**e”</i>.<br/>
    /// <i>m</i> -- mantissa, integer number, positive or negative,<br/>
    /// <i>e</i> -- exponent (power of 10), integer number, positive or negative.<br/>
    /// </summary>
    public class DFloat : IComparable<DFloat>, IEquatable<DFloat>
    {
        const int MaxExponentBytes = 32; // max bytes for mE
        static readonly BigInteger Bi2 = 2;
        static readonly BigInteger Bi5 = 5;
        static readonly BigInteger Bi10 = 10;
        const int BucketSize = 100;
        static readonly BigInteger TenPowBucket = BigInteger.Pow( Bi10, BucketSize );


        public BigInteger M { get; private init; }
        public BigInteger E { get; private init; } // (power of 10)

        public bool IsNegative => M < 0;
        public bool IsZero => M.IsZero;
        public bool QuickTestOne => M.IsOne && E.IsZero;
        public bool QuickTestMinusOne => E.IsZero && M == -1;
        public bool IsOne( ) { return QuickTestOne || Equals( One ); }
        public bool IsMinusOne( ) { return QuickTestMinusOne || Equals( MinusOne ); }
        public bool IsPositiveOrZero => M >= 0;
        public bool IsPositiveNonZero => M > 0;
        public bool IsApprox { get; private init; }

        public static DFloat Zero => new( 0 );
        public static DFloat One => new( 1 );
        public static DFloat MinusOne => new( -1 );
        public static DFloat Two => new( 2 );
        public static DFloat Ten => new( 1, 1 );
        public static DFloat Half => new( 5, -1 );
        public static DFloat MinusHalf => new( -5, -1 );
        public static DFloat Four => new( 4 );

        #region Special constants

        // from WolframAlpha:
        static readonly string Lb10Text = "3.3219280948873623478703194294893901758648313930245806120547563958159347766086252158501397433593701550996573717102502518268240969842635268882753027729986553938519513526575055686430176091900248916669414333740119031241873751097158664675401791896558067358307797";
        static readonly Lazy<(BigInteger n, BigInteger d)> Lb10Fraction = new( ( ) =>
        {
            int dt = Lb10Text.IndexOf( '.' );
            string s = Lb10Text.Remove( dt, 1 );

            return (BigInteger.Parse( s ), BigInteger.Pow( Bi10, s.Length - dt ));
        } );

        #endregion

        public DFloat( )
        {
            M = BigInteger.Zero;
            E = BigInteger.Zero;
            IsApprox = false;
        }

        public DFloat( BigInteger m )
        {
            M = m;
            E = BigInteger.Zero;
            IsApprox = false;
        }

        public DFloat( BigInteger m, BigInteger e )
        {
            M = m;
            E = e;
            IsApprox = false;
        }

        public DFloat( BigInteger m, BigInteger e, bool isApprox )
        {
            M = m;
            E = e;
            IsApprox = isApprox;
        }

        public DFloat AsApprox( )
        {
            if( IsApprox ) return this;

            return new DFloat( M, E, isApprox: true );
        }

        public DFloat AsNonApprox( )
        {
            if( !IsApprox ) return this;

            return new DFloat( M, E, isApprox: false );
        }

        public DFloat UnionApprox( bool approx )
        {
            if( IsApprox || !approx ) return this;

            return new DFloat( M, E, isApprox: true );
        }

        public DFloat UnionApprox( DFloat y )
        {
            return UnionApprox( y.IsApprox );
        }

        public DFloat UnionApprox( DFloat y1, DFloat y2 )
        {
            return UnionApprox( y1.IsApprox || y2.IsApprox );
        }

        /// <summary>
        /// Move the training zeroes from M to E. (Trim M, increase E).
        /// </summary>
        /// <returns></returns>
        public DFloat Trim( )
        {
            if( M.IsZero ) return this;
            if( !M.IsEven ) return this; // (means it is also not divisible by 10, so no trailing zeroes)

            var t = FractionUtilities.TrimZeroes( ICancellable.NonCancellable, M );

            DFloat f = new( t.x, E + t.e, IsApprox );

            Debug.Assert( f.Equals( this ) );

            return f;
        }

        /// <summary>
        /// For debugging purposes. To watch values in Debugger.
        /// Move trailing zeroes from or to M, adjusting E.
        /// </summary>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public DFloat Beautify( int maxBytes = 100 ) // for debugging
        {
            if( E == 0 ) return this;

            BigInteger m = M;
            BigInteger e = E;

            if( e < 0 )
            {
                // 123000000e-2 ==> 1230000
                // 12300e-7 ==> 123e-5

                do
                {
                    (BigInteger new_m, BigInteger r) = BigInteger.DivRem( m, Bi10 );

                    if( !r.IsZero ) break;

                    m = new_m;
                    ++e;
                }
                while( e < 0 );

                return new DFloat( m, e, IsApprox );
            }

            while( e > 0 && m.GetByteCount( ) <= maxBytes ) // 123e4 ==> 1230000
            {
                m *= Bi10;
                --e;
            }

            if( m.GetByteCount( ) > maxBytes ) // 12300000...00[e+YYY] ==> 123e+XXX if trailing zeroes (including previous 'while') make the value too large
            {
                var t = FractionUtilities.TrimZeroes( ICancellable.NonCancellable, M );

                m = t.x;
                e += t.e;
            }

            return new DFloat( m, e, IsApprox );
        }

        static (int e, BigInteger p, BigInteger hp) EvalPow10( long byteCount )
        {
            switch( byteCount )
            {
            case 0: return (0, 0, 0);
            case 1: return (2, 100, 100 / 2); // 255 
            case 2: return (4, 10_000, 10_000 / 2); // 65535
            case 3: return (7, 10_000_000, 10_000_000 / 2); // 16777215
            case 4: return (9, 1_000_000_000, 1_000_000_000 / 2); // 4294967295
            default:
            {
                long bits = byteCount * 8;
                (BigInteger n, BigInteger d) lb10 = Lb10Fraction.Value;
                BigInteger be = bits * lb10.d / lb10.n;
                int e = (int)be;
                BigInteger p = BigInteger.Pow( Bi10, e );

                return (e, p, p / 2);
            }
            }
            ;
        }

        /// <summary>
        /// Reduce the large values, finding an approximation.
        /// </summary>
        /// <param name="maxFullBytes">The target number of bytes for M.</param>
        /// <returns></returns>
        public DFloat Reduce( int maxFullBytes )
        {
            ArgumentOutOfRangeException.ThrowIfLessThan( maxFullBytes, 1 );

            // NOTE. The result will have less than 'maxFullBytes + 1' bytes. For example, 256 (2 bytes) will be returned unchanged if maxBytes is 1

            long bit_length = M.GetBitLength( );
            long max_bit_length = maxFullBytes * 8L;

            if( bit_length <= max_bit_length ) return this;

            long extra_bit_length = bit_length - max_bit_length;
            long extra_bytes = extra_bit_length / 8;

            if( extra_bytes == 0 ) return this;

            (int e, BigInteger divide_by, BigInteger divide_by_h) eval_pow10 = EvalPow10( extra_bytes );
            Debug.Assert( eval_pow10.e > 0 );

            (BigInteger new_m, BigInteger r) = BigInteger.DivRem( M, eval_pow10.divide_by );

            bool is_approx = !r.IsZero;

            // round
            if( r >= eval_pow10.divide_by_h ) ++new_m;
            else if( r <= -eval_pow10.divide_by_h ) --new_m;

            BigInteger new_e = E + eval_pow10.e;

            return new DFloat( new_m, new_e, IsApprox || is_approx );
        }

        #region Arithmetics

        /// <summary>
        /// Multiplication.
        /// </summary>
        /// <param name="f1"></param>
        /// <param name="f2"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="OverflowException"></exception>
        public static DFloat Mul( DFloat f1, DFloat f2, int maxBytes )
        {
            if( f1.IsZero || f2.IsZero ) return Zero.UnionApprox( f1.IsApprox && f2.IsApprox ); // (No 'IsApprox' if at least one precise zero)

            DFloat p = new( f1.M * f2.M, f1.E + f2.E, f1.IsApprox || f2.IsApprox );
            if( p.E.GetByteCount( ) > MaxExponentBytes ) throw new OverflowException( "Resulting exponent too large." );
            DFloat r = p.Reduce( maxBytes );

            return r;
        }

        /// <summary>
        /// Division.
        /// </summary>
        /// <param name="f1"></param>
        /// <param name="f2"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="DivideByZeroException"></exception>
        /// <exception cref="OverflowException"></exception>
        public static DFloat Div( DFloat f1, DFloat f2, int maxBytes )
        {
            if( f2.IsZero ) throw new DivideByZeroException( );
            if( f1.IsZero ) return Zero.UnionApprox( f1 );

            f1 = f1.Trim( );
            f2 = f2.Trim( );

            return DivInternal( f1, f2, maxBytes );

            static DFloat DivInternal( DFloat f1, DFloat f2, int maxBytes )
            {
                BigInteger e = f1.E - f2.E;

                BigInteger m1 = BigInteger.Abs( f1.M );
                BigInteger m2 = BigInteger.Abs( f2.M );
                BigInteger m;

                (m, BigInteger r) = BigInteger.DivRem( m1, m2 );

                bool is_approx = false;

                for( ; !r.IsZero; )
                {
                    r *= Bi10;

                    (BigInteger q, r) = BigInteger.DivRem( r, m2 );

                    Debug.Assert( q < 10 );

                    m = m * Bi10 + q;
                    --e;

                    if( m.GetByteCount( ) > maxBytes + 2 )
                    {
                        if( r > m2 ) ++m;

                        is_approx = true;

                        break;
                    }
                }

                if( e.GetByteCount( ) > MaxExponentBytes ) throw new OverflowException( "Resulting exponent too large." );

                return new DFloat( f1.M < 0 != f2.M < 0 ? -m : m, e, f1.IsApprox || f2.IsApprox || is_approx ).Reduce( maxBytes );
            }
        }

        #endregion

        #region Conversions

        /// <summary>
        /// Convert to double (if possible).
        /// </summary>
        /// <returns></returns>
        public double ToDouble( )
        {
            DFloat t = Trim( );
            double de = checked((double)t.E);
            double d = checked((double)t.M) * Math.Pow( 10, de );

            return d;
        }

        #endregion

        #region IComparable<DFloat>

        /// <summary>
        /// Compare with another number.
        /// </summary>
        /// <param name="other"></param>
        /// <returns>negative integer, zero or positive integer according to common comparison.</returns>
        public int CompareTo( DFloat? other )
        {
            if( other == null ) return -1;
            if( ReferenceEquals( this, other ) ) return 0;

            int s = M.Sign.CompareTo( other.M.Sign );

            if( s != 0 ) return s;

            if( M.IsZero || other.M.IsZero ) return M.CompareTo( other.M );
            if( M == other.M ) return E.CompareTo( other.E ) * M.Sign;
            if( E == other.E ) return M.CompareTo( other.M );

            BigInteger m1, e1;
            BigInteger m2, e2;

            // m1, e1 -- number having greater E; m2, e2 -- the other number

            if( E > other.E )
            {
                m1 = M;
                e1 = E;
                m2 = other.M;
                e2 = other.E;
            }
            else
            {
                Debug.Assert( E < other.E );

                m1 = other.M;
                e1 = other.E;
                m2 = M;
                e2 = E;
            }

            for(; ; )
            {
                BigInteger diff_e = e1 - e2;
                Debug.Assert( diff_e > 0 );

                int sh = unchecked((int)BigInteger.Min( diff_e, 10 ));

                m1 *= BigInteger.Pow( Bi10, sh );
                e1 -= sh;

                if( e1 == e2 )
                {
                    return E > other.E ? m1.CompareTo( m2 ) : m2.CompareTo( m1 );
                }

                if( m1 > 0 )
                {
                    if( m1 >= m2 )
                    {
                        return E > other.E ? +1 : -1;
                    }
                }
                else
                {
                    if( m1 <= m2 )
                    {
                        return E > other.E ? -1 : +1;
                    }
                }
            }
        }

        #endregion

        #region IEquatable<DFloat>

        /// <summary>
        /// Check if this number is equel with another one.
        /// </summary>
        /// <param name="other"></param>
        /// <returns></returns>
        public bool Equals( DFloat? other )
        {
            return CompareTo( other ) == 0;
        }

        #endregion

        #region Overrides

        public override bool Equals( object? obj )
        {
            DFloat? a = obj as DFloat;
            if( a != null ) return Equals( a );

            return base.Equals( obj );
        }

        public override int GetHashCode( )
        {
            DFloat t = Trim( );

            return HashCode.Combine( t.M, t.E );
        }

        public override string? ToString( )
        {
            if( E.IsZero )
            {
                return $"{M:D}";
            }
            else
            {
                return $"{M:D}e{E:D}";
            }
        }

        #endregion
    }
}
