using System.Collections.Concurrent;
using System.Diagnostics;
using System.Globalization;
using System.Numerics;
using System.Text;

namespace RationalApproximationLibrary
{
    // NOTE. Unused code is almost deleted.

    public class Fraction : IEquatable<Fraction>, IComparable<Fraction>
    {
        static readonly BigInteger Bi2 = 2;
        static readonly BigInteger Bi5 = 5;
        static readonly BigInteger BiMinus5 = -5;
        static readonly BigInteger Bi10 = 10;

        enum KindEnum
        {
            Undefined,
            Normal,
            NegativeInfinity,
            PositiveInfinity,
        }

        KindEnum Kind { get; init; } = KindEnum.Undefined;

        // The fraction is: N / D * 10^E
        // N, D and E are only meaningful if Kind is Normal
        public BigInteger N { get; private init; } // numerator, +/-
        public BigInteger D { get; private init; } // denominator, always > 0
        public BigInteger E { get; private init; } // exponent, +/-

        public bool IsUndefined => Kind == KindEnum.Undefined;
        public bool IsNormal => Kind == KindEnum.Normal;
        public bool IsNegativeInfinity => Kind == KindEnum.NegativeInfinity;
        public bool IsPositiveInfinity => Kind == KindEnum.PositiveInfinity;
        public bool IsAnyInfinity => IsPositiveInfinity || IsNegativeInfinity;
        public bool IsZero => IsNormal && N.IsZero;
        public bool IsNegative => IsNormal && N < 0;
        public bool IsNegativeOrZero => IsNormal && N <= 0;
        public bool IsPositiveNonZero => IsNormal && N > 0;
        public bool IsPositiveOrZero => IsNormal && N >= 0;

        public bool QuickTestOne => IsNormal && E.IsZero && N == D;
        public bool QuickTestMinusOne => IsNormal && E.IsZero && -N == D;

        public bool IsOne( ICancellable cnc ) => IsNormal && ( E.IsZero && N == D || Equals( cnc, One ) );
        public bool IsMinusOne( ICancellable cnc ) => IsNormal && ( E.IsZero && -N == D || Equals( cnc, MinusOne ) );

        public bool IsApprox { get; private init; }
        public bool IsSimplified { get; private init; }


        public static Fraction Undefined { get; } = new( ) { Kind = KindEnum.Undefined, IsApprox = false, IsSimplified = true, };
        public static Fraction NegativeInfinity { get; } = new( ) { Kind = KindEnum.NegativeInfinity, IsApprox = false, IsSimplified = true, };
        public static Fraction PositiveInfinity { get; } = new( ) { Kind = KindEnum.PositiveInfinity, IsApprox = false, IsSimplified = true, };
        public static Fraction Zero { get; } = new Fraction( 0, 1, 0, isApprox: false, isSimplified: true );
        public static Fraction Quarter { get; } = new Fraction( 1, 4, 0, isApprox: false, isSimplified: true );
        public static Fraction MinusQuarter { get; } = new Fraction( -1, 4, 0, isApprox: false, isSimplified: true );
        public static Fraction Half { get; } = new Fraction( 1, 2, 0, isApprox: false, isSimplified: true );
        public static Fraction MinusHalf { get; } = new Fraction( -1, 2, 0, isApprox: false, isSimplified: true );
        public static Fraction One { get; } = new Fraction( 1, 1, 0, isApprox: false, isSimplified: true );
        public static Fraction MinusOne { get; } = new Fraction( -1, 1, 0, isApprox: false, isSimplified: true );
        public static Fraction Two { get; } = new Fraction( 2, 1, 0, isApprox: false, isSimplified: true );
        public static Fraction Four { get; } = new Fraction( 4, 1, 0, isApprox: false, isSimplified: true );
        public static Fraction Ten { get; } = new Fraction( 10, 1, 0, isApprox: false, isSimplified: true );

        #region Common constants

        // from WolframAlpha:
        const string PiString = "3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587";
        static readonly Lazy<Fraction> PiLarge = new( ( ) => TryParse( PiString )!.AsApprox( ) );
        public static Fraction Pi => PiLarge.Value;

        // from WolframAlpha, "exp(1)":
        const string EulerNumberString = "2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901157383418793070215408914993488416750924476146066808226480016847741185374234544243710753907774499206955170";
        static readonly Lazy<Fraction> EulerNumberLarge = new( ( ) => TryParse( EulerNumberString.Trim( ) )!.AsApprox( ) );
        public static Fraction EulerNumber => EulerNumberLarge.Value;

        #endregion

        Fraction( )
        {
            Debug.Assert( Kind == KindEnum.Undefined );
        }

        public Fraction( BigInteger n, BigInteger d, BigInteger e, bool isApprox, bool isSimplified )
        {
            if( d < 0 ) throw new InvalidOperationException( "Denominator cannot be negative" );
            if( d.IsZero ) throw new InvalidOperationException( "Denominator cannot be zero" );

            Kind = KindEnum.Normal;
            N = n;
            D = d;
            E = e;
            IsApprox = isApprox;
            IsSimplified = isSimplified;
        }

        public Fraction( BigInteger n, BigInteger d, BigInteger e )
            : this( n, d, e, isApprox: false, isSimplified: false )
        {
        }

        public Fraction( BigInteger n, BigInteger d )
            : this( n, d, BigInteger.Zero, isApprox: false, isSimplified: false )
        {
        }

        public Fraction( BigInteger n )
            : this( n, BigInteger.One, BigInteger.Zero, isApprox: false, isSimplified: false )
        {
        }

        public Fraction( Fraction y )
        {
            Kind = y.Kind;
            N = y.N;
            D = y.D;
            E = y.E;
            IsApprox = y.IsApprox;
            IsSimplified = y.IsSimplified;
        }

        public Fraction AsApprox( )
        {
            if( !IsNormal || IsApprox ) return this;

            return new Fraction( N, D, E, isApprox: true, isSimplified: IsSimplified );
        }

        public Fraction AsNonApprox( )
        {
            if( !IsNormal || !IsApprox ) return this;

            return new Fraction( N, D, E, isApprox: false, isSimplified: IsSimplified );
        }

        public Fraction UnionApprox( bool approx )
        {
            if( !IsNormal || IsApprox || !approx ) return this;

            return new Fraction( N, D, E, isApprox: true, isSimplified: IsSimplified );
        }

        public Fraction UnionApprox( Fraction y )
        {
            return UnionApprox( y.IsApprox );
        }

        public Fraction UnionApprox( Fraction y1, Fraction y2 )
        {
            return UnionApprox( y1.IsApprox || y2.IsApprox );
        }

        /// <summary>
        /// Use Greatest Common Divisor, and move trailing zeroes from E to N or D as much as possible.
        /// </summary>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <remarks>The value is not affected.</remarks>
        public Fraction Simplify( CalculationContext ctx )
        {
            if( IsSimplified ) return this;

            Debug.Assert( IsNormal ); // (non-normal numbers should be considered simplified)

            BigInteger n = N;
            BigInteger d = D;
            BigInteger e = E;

            // if n is too large, try to move zeroes from n to e

            while( n < ctx.MinVal || n > ctx.MaxVal )
            {
                var (q, r) = BigInteger.DivRem( n, Bi10 );

                if( !r.IsZero ) break;

                n = q;
                ++e;

                ctx.Cnc.TryThrow( );
            }

            // if d is too large, try to move zeroes from d to e

            while( d > ctx.MaxVal )
            {
                var (q, r) = BigInteger.DivRem( d, Bi10 );

                if( !r.IsZero ) break;

                d = q;
                --e;

                ctx.Cnc.TryThrow( );
            }

            // move zeroes from e to n

            while( e > 0 && n >= ctx.MinValDiv10 && n <= ctx.MaxValDiv10 )
            {
                n *= Bi10;
                --e;

                ctx.Cnc.TryThrow( );
            }

            // move zeroes from e to d

            while( e < 0 && d <= ctx.MaxValDiv10 )
            {
                d *= Bi10;
                ++e;

                ctx.Cnc.TryThrow( );
            }

            // simplify n and d using Greatest Common Divisor

            BigInteger gcd = FractionUtilities.GreatestCommonDivisor( ctx.Cnc, n < 0 ? -n : n, d );

            n /= gcd;
            d /= gcd;

            // move zeroes from e to n as much as possible

            while( e > 0 && n >= ctx.MinValDiv10 && n <= ctx.MaxValDiv10 )
            {
                n *= Bi10;
                --e;

                ctx.Cnc.TryThrow( );
            }

            // move zeroes from e to d as much as possible

            while( e < 0 && d <= ctx.MaxValDiv10 )
            {
                d *= Bi10;
                ++e;

                ctx.Cnc.TryThrow( );
            }

            return new Fraction( n, d, e, isApprox: IsApprox, isSimplified: true );
        }

        /// <summary>
        /// Move all trailing zeroes from N and D to E.
        /// </summary>
        /// <param name="cnc"></param>
        /// <returns></returns>
        public Fraction TrimZeroes( ICancellable cnc )
        {
            if( !IsNormal ) return this;

            (BigInteger n, int en) = FractionUtilities.TrimZeroes( cnc, N );
            (BigInteger d, int ed) = FractionUtilities.TrimZeroesGE0( cnc, D );
            BigInteger e = E + en - ed;

            return new Fraction( n, d, e, isApprox: IsApprox, isSimplified: false );
        }

        /// <summary>
        /// Get the nearest fraction. Only for ]0..1].
        /// </summary>
        /// <param name="cnc"></param>
        /// <param name="N"></param>
        /// <param name="D"></param>
        /// <param name="maxVal"></param>
        /// <returns></returns>
        static (BigInteger n, BigInteger d) FareyInternal( ICancellable cnc, BigInteger N, BigInteger D, BigInteger maxVal )
        {
            // must be ]0..1]
            Debug.Assert( N > 0 );
            Debug.Assert( D > 0 );
            Debug.Assert( N <= D );

            if( N == D ) return (1, 1);

            /*

            Based on https://www.johndcook.com/rational_approximation.html

             */

            BigInteger n0 = 0, n1 = 1;
            BigInteger d0 = 1, d1 = 1;

            for(; ; )
            {
                cnc.TryThrow( );

                BigInteger dm = d0 + d1;

                if( dm > maxVal ) break;

                BigInteger nm = n0 + n1;

                switch( ( nm * D ).CompareTo( dm * N ) )
                {
                case < 0: // nm/dm < x
                {
                    // Debug.WriteLine( $"<: {nm}/{dm}" );

                    BigInteger k_max = ( maxVal - d0 ) / d1;
                    Debug.Assert( k_max >= 0 );

                    BigInteger t = D * n1 - N * d1;
                    BigInteger k = t == 0 ? k_max : ( N * d0 - D * n0 ) / t;
                    Debug.Assert( k >= 0 );

                    BigInteger k_min = BigInteger.Min( k_max, k );

                    if( k_min.IsOne || k_min.IsZero )
                    {
                        n0 = nm;
                        d0 = dm;
                    }
                    else
                    {
                        n0 += n1 * k_min;
                        d0 += d1 * k_min;
                    }
                }
                break;
                case > 0: // nm/dm > x
                {
                    //Debug.WriteLine( $">: {nm}/{dm}" );

                    BigInteger k_max = ( maxVal - d1 ) / d0;
                    Debug.Assert( k_max >= 0 );

                    BigInteger t = D * n0 - N * d0;
                    BigInteger k = t == 0 ? k_max : ( N * d1 - D * n1 ) / t;
                    Debug.Assert( k >= 0 );

                    BigInteger k_min = BigInteger.Min( k_max, k );

                    if( k_min.IsOne || k_min.IsZero )
                    {
                        n1 = nm;
                        d1 = dm;
                    }
                    else
                    {
                        n1 += n0 * k_min;
                        d1 += d0 * k_min;
                    }
                }
                break;
                default: // nm/dm == x
                    return (nm, dm);
                }
            }

            if( 2 * N * d1 * d0 <= ( n1 * d0 + n0 * d1 ) * D )
            {
                return (n0, d0);
            }
            else
            {
                return (n1, d1);
            }
        }

        /// <summary>
        /// Get the nearest fraction, were N and D do not exceed a limit. 
        /// Avoid the results that include an exponent.
        /// </summary>
        /// <param name="cnc"></param>
        /// <param name="maxVal"></param>
        /// <returns></returns>
        public Fraction ReduceNoE( ICancellable cnc, BigInteger maxVal )
        {
            if( maxVal <= 0 ) throw new ArgumentOutOfRangeException( nameof( maxVal ), $"'{nameof( maxVal )}' must be positive" );

            if( !IsNormal ) return this;
            if( IsZero ) return this;
            if( QuickTestOne ) return One.UnionApprox( IsApprox );
            if( QuickTestMinusOne ) return MinusOne.UnionApprox( IsApprox );

            if( E.IsZero && N >= -maxVal && N <= maxVal && D <= maxVal )
            {
                // nothing to reduce

                return this;
            }

            if( this.CompareTo( cnc, new Fraction( -1, 1, -1000 ) ) > 0 && this.CompareTo( cnc, new Fraction( 1, 1, -1000 ) ) < 0 )
            {
                // too small, close to zero

                return Zero;
            }

            if( this.CompareTo( cnc, MinusOne ) > 0 && this.CompareTo( cnc, One ) < 0 ) // ]-1...+1[
            {
                // 'this' is ]-1...+1[, not too close to zero, abs(E) is not too large

                BigInteger n1 = BigInteger.Abs( N );
                BigInteger d1 = D;
                BigInteger e1 = E;

                // eliminate E

                while( e1 < 0 )
                {
                    d1 *= Bi10;
                    ++e1;
                }

                while( e1 > 0 )
                {
                    n1 *= Bi10;
                    --e1;
                }

                Debug.Assert( e1 == 0 );

                (BigInteger n, BigInteger d) result = FareyInternal( cnc, n1, d1, maxVal );

                return new Fraction( IsNegative ? -result.n : result.n, result.d, BigInteger.Zero, isApprox: true, isSimplified: false );
            }

            if( this.CompareTo( cnc, new Fraction( -1, 1, 1000 ) ) > 0 && this.CompareTo( cnc, new Fraction( 1, 1, 1000 ) ) < 0 ) // not in [-1...+1], but not too large (E not too large)
            {
                BigInteger n1 = BigInteger.Abs( N );
                BigInteger d1 = D;
                BigInteger e1 = E;

                // eliminate E

                while( e1 < 0 )
                {
                    d1 *= Bi10;
                    ++e1;
                }

                while( e1 > 0 )
                {
                    n1 *= Bi10;
                    --e1;
                }

                Debug.Assert( e1 == 0 );
                Debug.Assert( n1 > d1 );

                (BigInteger n, BigInteger d) result1 = FareyInternal( cnc, d1, n1, maxVal );

                Debug.Assert( result1.n < result1.d );

                if( !result1.n.IsZero )
                {
                    return new Fraction( IsNegative ? -result1.d : result1.d, result1.n, BigInteger.Zero, isApprox: true, isSimplified: false );
                }
                // else the number is too large
            }

            // the number is too large

            return new Fraction( IsNegative ? -maxVal : maxVal, BigInteger.One, BigInteger.Zero, isApprox: true, isSimplified: false );
        }

        /// <summary>
        /// Get the nearest fraction, were N and D do not exceed a limit.
        /// </summary>
        /// <param name="cnc"></param>
        /// <param name="maxVal"></param>
        /// <returns></returns>
        public Fraction Reduce( ICancellable cnc, BigInteger maxVal )
        {
            if( maxVal <= 0 ) throw new ArgumentOutOfRangeException( nameof( maxVal ), $"'{nameof( maxVal )}' must be positive" );

            if( !IsNormal ) return this;
            if( IsZero ) return this;
            if( QuickTestOne ) return One.UnionApprox( IsApprox );
            if( QuickTestMinusOne ) return MinusOne.UnionApprox( IsApprox );

            if( N >= -maxVal && N <= maxVal && D <= maxVal )
            {
                // TODO: move zeroes to E?
                // TODO: simplify?

                return this;
            }

            var this_Abs_NDE = FractionUtilities.Abs( ToNDE( ) );
            BigInteger abs_N = this_Abs_NDE.n;

            if( abs_N < D )
            {
                BigInteger n1 = abs_N;
                BigInteger d1 = D;
                BigInteger e1 = E;

                do
                {
                    n1 *= Bi10; //
                    --e1;
                    cnc.TryThrow( );
                } while( n1 < d1 );

                (BigInteger d, BigInteger n) result1 = FareyInternal( cnc, d1, n1, maxVal );
                Debug.Assert( result1.d > 0 );
                Debug.Assert( result1.n > 0 );
                Debug.Assert( E >= e1 );
                (BigInteger n, BigInteger d, BigInteger e) diff1 = FractionUtilities.Abs( FractionUtilities.DiffSmallDiffE( this_Abs_NDE, (result1.n, result1.d, e1) ) );

                if( diff1.n.IsZero ) return new Fraction( IsNegative ? -result1.n : result1.n, result1.d, e1, isApprox: IsApprox, isSimplified: false );

                BigInteger n2 = n1;
                BigInteger d2 = d1 * Bi10;
                BigInteger e2 = e1 + 1;

                (BigInteger n, BigInteger d) result2 = FareyInternal( cnc, n2, d2, maxVal );
                Debug.Assert( result2.d > 0 );
                Debug.Assert( E >= e2 );
                (BigInteger n, BigInteger d, BigInteger e) diff2 = FractionUtilities.Abs( FractionUtilities.DiffSmallDiffE( this_Abs_NDE, (result2.n, result2.d, e2) ) );

                if( diff2.n.IsZero ) return new Fraction( IsNegative ? -result2.n : result2.n, result2.d, e2, isApprox: IsApprox, isSimplified: false );

                if( FractionUtilities.Compare( diff1, diff2 ) <= 0 )
                {
                    return new Fraction( IsNegative ? -result1.n : result1.n, result1.d, e1, isApprox: true, isSimplified: false );
                }
                else
                {
                    return new Fraction( IsNegative ? -result2.n : result2.n, result2.d, e2, isApprox: true, isSimplified: false );
                }
            }
            else
            {
                BigInteger n1 = abs_N;
                BigInteger d1 = D;
                BigInteger e1 = E;

                do
                {
                    d1 *= Bi10;
                    ++e1;
                    cnc.TryThrow( );
                } while( n1 > d1 );

                (BigInteger n, BigInteger d) result1 = FareyInternal( cnc, n1, d1, maxVal );
                Debug.Assert( result1.d > 0 );
                Debug.Assert( E <= e1 );
                (BigInteger n, BigInteger d, BigInteger e) diff1 = FractionUtilities.Abs( FractionUtilities.DiffSmallDiffE( this_Abs_NDE, (result1.n, result1.d, e1) ) );

                if( diff1.n.IsZero ) return new Fraction( IsNegative ? -result1.n : result1.n, result1.d, e1, isApprox: IsApprox, isSimplified: false );

                BigInteger n2 = n1 * Bi10;
                BigInteger d2 = d1;
                BigInteger e2 = e1 - 1;

                (BigInteger d, BigInteger n) result2 = FareyInternal( cnc, d2, n2, maxVal );
                Debug.Assert( result2.d > 0 );
                Debug.Assert( result2.n > 0 );
                Debug.Assert( E <= e2 );
                (BigInteger n, BigInteger d, BigInteger e) diff2 = FractionUtilities.Abs( FractionUtilities.DiffSmallDiffE( this_Abs_NDE, (result2.n, result2.d, e2) ) );

                if( diff2.n.IsZero ) return new Fraction( IsNegative ? -result2.n : result2.n, result2.d, e2, isApprox: IsApprox, isSimplified: false );

                if( FractionUtilities.Compare( diff1, diff2 ) <= 0 )
                {
                    return new Fraction( IsNegative ? -result1.n : result1.n, result1.d, e1, isApprox: true, isSimplified: false );
                }
                else
                {
                    return new Fraction( IsNegative ? -result2.n : result2.n, result2.d, e2, isApprox: true, isSimplified: false );
                }
            }
        }

        /// <summary>
        /// Reduce large numerator and denominator, and adjust exponent. (Find the best approximation).
        /// </summary>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public Fraction Reduce( CalculationContext ctx )
        {
            return Reduce( ctx.Cnc, ctx.MaxVal );
        }


        #region Arithmetics

        /// <summary>
        /// Absolute value.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Abs( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return PositiveInfinity;
            if( value.IsPositiveInfinity ) return PositiveInfinity;

            Debug.Assert( value.IsNormal );

            if( value.IsPositiveOrZero ) return value;

            Debug.Assert( value.IsNegative );

            return new Fraction( -value.N, value.D, value.E, isApprox: value.IsApprox, isSimplified: value.IsSimplified );
        }

        /// <summary>
        /// Negation.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Neg( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return PositiveInfinity;
            if( value.IsZero ) return value;
            if( value.IsPositiveInfinity ) return NegativeInfinity;

            Debug.Assert( value.IsNormal );

            return new Fraction( -value.N, value.D, value.E, isApprox: value.IsApprox, isSimplified: value.IsSimplified );
        }

        /// <summary>
        /// Addition.
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Add( Fraction left, Fraction right, CalculationContext ctx )
        {
            if( left.IsUndefined ) return Undefined;
            if( right.IsUndefined ) return Undefined;

            // -∞ + -∞ = -∞
            // -∞ +  ∞ = undefined
            // -∞ +  3 = -∞
            //  ∞ + -∞ = undefined
            //  ∞ +  ∞ = ∞
            //  ∞ +  3 = ∞

            if( left.IsNegativeInfinity )
            {
                if( right.IsPositiveInfinity ) return Undefined;

                return NegativeInfinity;
            }

            if( left.IsPositiveInfinity )
            {
                if( right.IsNegativeInfinity ) return Undefined;

                return PositiveInfinity;
            }

            Debug.Assert( left.IsNormal );

            if( right.IsPositiveInfinity ) return PositiveInfinity;
            if( right.IsNegativeInfinity ) return NegativeInfinity;

            Debug.Assert( right.IsNormal );

            return AddInternal( left, right, ctx );
        }

        static Fraction AddInternal( Fraction left, Fraction right, CalculationContext ctx )
        {
            Debug.Assert( left.IsNormal );
            Debug.Assert( right.IsNormal );

            bool is_approx = left.IsApprox || right.IsApprox;

            if( left.IsZero ) return right.UnionApprox( is_approx ); // "0 + y"
            if( right.IsZero ) return left.UnionApprox( is_approx ); // "x + 0"

            if( ReferenceEquals( left, right ) ) // "x + x"
            {
                return new Fraction( left.N * 2, left.D, left.E, isApprox: is_approx, isSimplified: false ).Reduce( ctx );
            }

            BigInteger lcm = FractionUtilities.LeastCommonMultiple( ctx.Cnc, left.D, right.D );

            ctx.Cnc.TryThrow( );

            BigInteger n = lcm / left.D * left.N;
            BigInteger new_y_N = lcm / right.D * right.N;

            BigInteger d = lcm;
            BigInteger e;

            BigInteger diff_e = left.E - right.E;

            if( diff_e >= 0 )
            {
                (bool is_approx1, BigInteger n1, BigInteger e1) = MulPow10AddLimited( n, diff_e, new_y_N, ctx.Cnc, ctx.MinVal, ctx.MaxVal ); //.............

                n = n1;
                e = right.E + e1;
                is_approx |= is_approx1;
            }
            else
            {
                (bool is_approx1, BigInteger n1, BigInteger e1) = MulPow10AddLimited( new_y_N, -diff_e, n, ctx.Cnc, ctx.MinVal, ctx.MaxVal ); //..........

                n = n1;
                e = left.E + e1;
                is_approx |= is_approx1;
            }

            return new Fraction( n, d, e, isApprox: is_approx, isSimplified: false ).Reduce( ctx );
        }

        internal static (bool isApprox, BigInteger x, BigInteger e) MulPow10AddLimited( BigInteger x1, BigInteger e1,
            BigInteger x2, ICancellable cnc, BigInteger minValue, BigInteger maxValue )
        {
            Debug.Assert( minValue < 0 );
            Debug.Assert( maxValue > 0 );
            Debug.Assert( minValue == -maxValue ); // only typical limits expected
            Debug.Assert( e1 >= 0 );

            if( x1.IsZero ) return (false, x2, 0);
            if( x2.IsZero ) return (false, x1, e1);

            if( x1 >= 0 )
            {
                for( ; e1 > 0; --e1 )
                {
                    var next_x1 = x1 * Bi10;
                    if( next_x1 > maxValue ) break;

                    x1 = next_x1;
                }
            }
            else
            {
                for( ; e1 > 0; --e1 )
                {
                    var next_x1 = x1 * Bi10;
                    if( next_x1 < minValue ) break;

                    x1 = next_x1;
                }
            }

            cnc.TryThrow( );

            BigInteger r = 0;
            bool is_approx = false;

            var ee = e1;

            while( ee > 0 )
            {
                (x2, r) = BigInteger.DivRem( x2, Bi10 ); // (+ divrem 10) ==> (+, +); (- divrem 10) ==> (-, -)

                if( !r.IsZero ) is_approx = true;

                --ee;

                if( x2.IsZero && r.IsZero ) break;
            }

            cnc.TryThrow( );

            if( r <= BiMinus5 || r >= Bi5 ) ++x2;

            x1 += x2;

            while( !x1.IsZero )
            {
                (r, BigInteger q) = BigInteger.DivRem( x1, Bi10 );

                if( !q.IsZero ) break;

                x1 = r;
                ++e1;
            }

            return (is_approx, x1, e1);
        }

        /// <summary>
        /// Multiplication.
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Mul( Fraction left, Fraction right, CalculationContext ctx )
        {
            if( left.IsUndefined ) return Undefined;
            if( right.IsUndefined ) return Undefined;

            // -∞ * -∞ =  ∞
            // -∞ *  ∞ = -∞
            // -∞ *  0 = undefined
            // -∞ * -3 =  ∞
            // -∞ *  3 = -∞
            //  ∞ *  ∞ =  ∞
            //  ∞ *  0 = undefined
            //  ∞ * -3 = -∞
            //  ∞ *  3 =  ∞

            if( left.IsZero )
            {
                if( right.IsNegativeInfinity || right.IsPositiveInfinity ) return Undefined;

                return Zero.UnionApprox( left ); // (no 'right')
            }

            if( right.IsZero )
            {
                if( left.IsNegativeInfinity || left.IsPositiveInfinity ) return Undefined;

                return Zero.UnionApprox( right ); // (no 'left')
            }

            if( left.IsNegativeInfinity )
            {
                if( right.IsNegativeInfinity ) return PositiveInfinity;
                if( right.IsPositiveInfinity ) return NegativeInfinity;
                if( right.IsNegative ) return PositiveInfinity;

                Debug.Assert( right.IsPositiveNonZero );

                return NegativeInfinity;
            }

            if( right.IsNegativeInfinity )
            {
                if( left.IsPositiveInfinity ) return NegativeInfinity;
                if( left.IsNegative ) return PositiveInfinity;

                Debug.Assert( left.IsPositiveNonZero );

                return NegativeInfinity;
            }

            if( left.IsPositiveInfinity )
            {
                if( right.IsNegativeInfinity ) return NegativeInfinity;
                if( right.IsPositiveInfinity ) return PositiveInfinity;
                if( right.IsNegative ) return NegativeInfinity;

                Debug.Assert( right.IsPositiveNonZero );

                return PositiveInfinity;
            }

            if( right.IsPositiveInfinity )
            {
                if( left.IsNegative ) return NegativeInfinity;

                Debug.Assert( left.IsPositiveNonZero );

                return PositiveInfinity;
            }

            Debug.Assert( left.IsNormal );
            Debug.Assert( right.IsNormal );
            Debug.Assert( !left.IsZero );
            Debug.Assert( !right.IsZero );

            left = left.Simplify( ctx );
            right = right.Simplify( ctx );

            return new Fraction( left.N * right.N, left.D * right.D, left.E + right.E, isApprox: left.IsApprox || right.IsApprox, isSimplified: false ).Reduce( ctx );
        }

        /// <summary>
        /// Division.
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Div( Fraction left, Fraction right, CalculationContext ctx )
        {
            if( left.IsUndefined ) return Undefined;
            if( right.IsUndefined ) return Undefined;

            // -∞ / -∞ = undefined
            // -∞ /  ∞ = undefined
            // -∞ /  0 = undefined (or complex infinity)
            // -∞ / -3 = ∞
            // -∞ /  3 = -∞
            //  ∞ /  -∞ = undefined
            //  ∞ /  ∞ = undefined
            //  ∞ /  0 = undefined (or complex infinity)
            //  ∞ / -3 = -∞
            //  ∞ / 3 = ∞
            //  3 / -∞ = 0
            //  3 / ∞ = 0
            //  3 / 0 = undefined (or complex infinity)
            //  0 / 0 = undefined
            //  0 / * = 0

            if( left.IsNegativeInfinity )
            {
                if( right.IsNegative ) return PositiveInfinity;
                if( right.IsPositiveNonZero ) return NegativeInfinity;

                return Undefined;
            }

            if( left.IsPositiveInfinity )
            {
                if( right.IsNegative ) return NegativeInfinity;
                if( right.IsPositiveNonZero ) return PositiveInfinity;

                return Undefined;
            }

            Debug.Assert( left.IsNormal );

            if( left.IsZero )
            {
                if( right.IsZero ) return Undefined;

                return left.UnionApprox( right );
            }

            if( right.IsAnyInfinity ) return Zero; // (no approx)

            Debug.Assert( right.IsNormal );

            if( right.IsZero ) return Undefined;

            return new Fraction( left.N * right.D * right.N.Sign, left.D * BigInteger.Abs( right.N ), left.E - right.E, isApprox: left.IsApprox || right.IsApprox, isSimplified: false ).Reduce( ctx );
        }

        /// <summary>
        /// Subtraction.
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Sub( Fraction left, Fraction right, CalculationContext ctx )
        {
            // TODO: expand?

            return Add( left, Neg( right, ctx ), ctx );
        }

        #endregion

        #region Conversions

        public static Fraction? TryParse( string s )
        {
            return FractionFormatter.TryParse( s );
        }

        /// <summary>
        /// Try to interpret this fraction as an integer.
        /// </summary>
        /// <param name="ctx"></param>
        /// <param name="integer"></param>
        /// <returns></returns>
        public bool TryGetInteger( CalculationContext ctx, out int integer )
        {
            if( !IsNormal )
            {
                integer = 0;

                return false;
            }

            var s = Simplify( ctx );

            if( s.E >= 0 && s.D == 1 && s.N >= int.MinValue && s.N <= int.MaxValue )
            {
                integer = unchecked((int)s.N);

                return true;
            }
            else
            {
                integer = 0;

                return false;
            }
        }

        /// <summary>
        /// Try to convert to double.
        /// </summary>
        /// <returns></returns>
        public double ToDouble( )
        {
            if( IsUndefined ) return double.NaN;
            if( IsNegativeInfinity ) return double.NegativeInfinity;
            if( IsPositiveInfinity ) return double.PositiveInfinity;
            if( IsZero ) return 0.0;
            if( QuickTestOne ) return 1.0;
            if( QuickTestMinusOne ) return -1.0;

            Debug.Assert( IsNormal );

            DFloat n = new( N, E );
            DFloat d = new( D );
            DFloat f = DFloat.Div( n, d, Math.Max( Math.Max( N.GetByteCount( ), D.GetByteCount( ) ), sizeof( double ) ) + 4 );

            return f.ToDouble( );
        }

        /// <summary>
        /// Extract numerator, denominator and exponent.
        /// </summary>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public (BigInteger n, BigInteger d, BigInteger e) ToNDE( )
        {
            if( !IsNormal ) throw new InvalidOperationException( );

            return (N, D, E);
        }

        public string ToRationalString( ICancellable cnc, int maxDigits )
        {
            return FractionFormatter.ToRationalString( cnc, this, maxDigits );
        }

        public string ToFloatString( ICancellable cnc, int maxDigits, bool group = false )
        {
            return FractionFormatter.ToFloatString( cnc, this, maxDigits, group );
        }

        #endregion


        #region Comparisons

        public bool Equals( ICancellable cnc, Fraction? y )
        {
            return !ReferenceEquals( y, null ) && CompareTo( cnc, y ) == 0;
        }

        public int CompareTo( ICancellable _, Fraction? y )
        {
            ArgumentNullException.ThrowIfNull( y );

            if( IsUndefined ) throw new NotSupportedException( $"“{nameof( CompareTo )}” cannot be used with undefined values." );
            if( y.IsUndefined ) throw new NotSupportedException( $"“{nameof( CompareTo )}” cannot be used with undefined values." );

            if( ReferenceEquals( this, y ) ) return 0;

            if( IsAnyInfinity ) throw new NotSupportedException( $"“{nameof( CompareTo )}” cannot be used with infinity." );
            if( y.IsAnyInfinity ) throw new NotSupportedException( $"“{nameof( CompareTo )}” cannot be used with infinity." );

            Debug.Assert( IsNormal );
            Debug.Assert( y.IsNormal );

            return FractionUtilities.Compare( ToNDE( ), y.ToNDE( ) );
        }

        public static bool operator ==( Fraction? left, Fraction? right )
        {
            if( left is null ) return right is null;

            return left.Equals( right );
        }

        public static bool operator ==( Fraction? left, BigInteger right )
        {
            if( left is null ) return false;

            return left.Equals( new Fraction( right ) );
        }

        public static bool operator !=( Fraction? left, Fraction? right )
        {
            return !( left == right );
        }

        public static bool operator !=( Fraction? left, BigInteger right )
        {
            return !( left == right );
        }

        public static bool operator <( Fraction? left, Fraction? right )
        {
            return left is null ? right is not null : left.CompareTo( right ) < 0;
        }

        public static bool operator <=( Fraction? left, Fraction? right )
        {
            return left is null || left.CompareTo( right ) <= 0;
        }

        public static bool operator >( Fraction? left, Fraction? right )
        {
            return left is not null && left.CompareTo( right ) > 0;
        }

        public static bool operator >=( Fraction? left, Fraction? right )
        {
            return left is null ? right is null : left.CompareTo( right ) >= 0;
        }

        #endregion


        #region IEquatable<Fraction>

        public bool Equals( Fraction? other )
        {
            return this.Equals( ICancellable.NonCancellable, other );
        }

        #endregion


        #region IComparable<Fraction>

        public int CompareTo( Fraction? other )
        {
            return CompareTo( ICancellable.NonCancellable, other );
        }

        #endregion


        #region Overrides

        public override bool Equals( object? obj )
        {
            if( obj == null ) return false;

            if( obj is Fraction y )
            {
                return Equals( y );
            }

            if( obj is BigInteger b )
            {
                return Equals( new Fraction( b ) );
            }

            try
            {
                long u = Convert.ToInt64( obj, CultureInfo.InvariantCulture );

                return Equals( new Fraction( u ) );
            }
            catch( OverflowException )
            {
                try
                {
                    ulong u = Convert.ToUInt64( obj, CultureInfo.InvariantCulture );

                    return Equals( new Fraction( u ) );
                }
                catch( OverflowException )
                {
                    // ignore
                }
                catch( InvalidCastException )
                {
                    // ignore
                }

            }
            catch( InvalidCastException )
            {
                // ignore
            }

            return false;
        }

        public override int GetHashCode( )
        {
            return Kind switch
            {
                KindEnum.Undefined => 0,
                KindEnum.NegativeInfinity => -1,
                KindEnum.PositiveInfinity => +1,
                KindEnum.Normal => GetHashCode( N, D, E ),
                _ => throw new InvalidOperationException( ),
            };

            static int GetHashCode( BigInteger n, BigInteger d, BigInteger e )
            {
                if( n == 0 ) return HashCode.Combine( 0, 1, 0 );

                ICancellable cnc = ICancellable.NonCancellable;

                bool is_negative = n < 0;
                if( is_negative ) n = -n;

                (n, BigInteger e_n) = FractionUtilities.TrimZeroes( cnc, n );
                (d, BigInteger e_d) = FractionUtilities.TrimZeroesGE0( cnc, d );
                e += e_n - e_d;

                BigInteger gcd = FractionUtilities.GreatestCommonDivisor( cnc, n, d );
                n /= gcd;
                d /= gcd;

                if( is_negative ) n = -n;

                return HashCode.Combine( n, d, e );
            }
        }

        public override string ToString( )
        {
            if( IsUndefined ) return IsApprox ? "≈Undefined" : "Undefined";
            if( IsPositiveInfinity ) return IsApprox ? "≈+Infinity" : "+Infinity";
            if( IsNegativeInfinity ) return IsApprox ? "≈-Infinity" : "-Infinity";

            Debug.Assert( IsNormal );

            if( IsZero ) return IsApprox ? "≈0" : "0";

            StringBuilder sb = new( );

            if( IsApprox ) sb.Append( '≈' );

            sb.Append( N.ToString( "D", CultureInfo.InvariantCulture ) );

            if( E != 0 )
            {
                sb.Append( 'e' ).Append( E.ToString( "+0;-0", CultureInfo.InvariantCulture ) );
            }

            if( D != 1 )
            {
                sb.Append( '/' ).Append( D.ToString( "D", CultureInfo.InvariantCulture ) );
            }

            return sb.ToString( );
        }

        #endregion
    }
}
