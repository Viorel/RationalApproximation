using System.Collections.Concurrent;
using System.Diagnostics;
using System.Globalization;
using System.Numerics;
using System.Text;

namespace RationalApproximationLibrary
{
    public class Fraction : IEquatable<Fraction>, IComparable<Fraction>
    {
        static readonly BigInteger Bi2 = 2;
        static readonly BigInteger Bi5 = 5;
        static readonly BigInteger BiMinus5 = -5;
        static readonly BigInteger Bi10 = 10;

        const int BucketSize = 100;
        static readonly BigInteger TenPowBucket = BigInteger.Pow( Bi10, BucketSize );

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

        #region Common constants

        // from WolframAlpha:
        const string PiString = "3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587";
        static readonly Lazy<Fraction> PiLarge = new( ( ) => TryParse( PiString )!.AsApprox( ) );
        static readonly ConcurrentDictionary<BigInteger /* maxVal */, Fraction> PiCached = new( );
        static readonly ConcurrentDictionary<BigInteger /* maxVal */, Fraction> TwoPiCached = new( );
        static readonly ConcurrentDictionary<BigInteger /* maxVal */, Fraction> PiOverTwoCached = new( );
        static readonly ConcurrentDictionary<BigInteger /* maxVal */, Fraction> PiOverFourCached = new( );

        public static Fraction GetPi( CalculationContext ctx )
        {
            return PiCached.GetOrAdd( ctx.MaxVal, mv => PiLarge.Value.Reduce( ctx.Cnc, mv ) );
        }

        public static Fraction Get2Pi( CalculationContext ctx )
        {
            return TwoPiCached.GetOrAdd( ctx.MaxVal, mv => new Fraction( PiLarge.Value.N * 2, PiLarge.Value.D, PiLarge.Value.E ).Reduce( ctx.Cnc, mv ) );
        }

        public static Fraction GetPiOverTwo( CalculationContext ctx )
        {
            return PiOverTwoCached.GetOrAdd( ctx.MaxVal, mv => new Fraction( PiLarge.Value.N, PiLarge.Value.D * 2, PiLarge.Value.E ).Reduce( ctx.Cnc, mv ) );
        }

        public static Fraction GetPiOverFour( CalculationContext ctx )
        {
            return PiOverFourCached.GetOrAdd( ctx.MaxVal, mv => new Fraction( PiLarge.Value.N, PiLarge.Value.D * 4, PiLarge.Value.E ).Reduce( ctx.Cnc, mv ) );
        }

        // from WolframAlpha, "exp(1)":
        const string EString = "2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901157383418793070215408914993488416750924476146066808226480016847741185374234544243710753907774499206955170";
        static readonly Lazy<Fraction> ELarge = new( ( ) => TryParse( EString.Trim( ) )!.AsApprox( ) );
        static readonly ConcurrentDictionary<BigInteger /* maxVal */, Fraction> ECached = new( );

        public static Fraction GetE( CalculationContext ctx )
        {
            return ECached.GetOrAdd( ctx.MaxVal, mv => ELarge.Value.Reduce( ctx.Cnc, mv ) );
        }

        // from WolframAlpha:
        const string Lb10String = "3.32192809488736234787031942948939017586483139302458061205475639581593477660862521585013974335937015509965737171025025182682409698426352688827530277299865539385195135265750556864301760919002489166694143337401190312418737510971586646754017918965580673583077968843272588327499252244890238355997641739413792800977";
        static readonly Lazy<Fraction> Lb10Large = new( ( ) => TryParse( Lb10String )!.AsApprox( ) );
        static readonly ConcurrentDictionary<BigInteger /* maxVal */, Fraction> Lb10Cached = new( );

        static Fraction GetLb10( CalculationContext ctx )
        {
            return Lb10Cached.GetOrAdd( ctx.MaxVal, mv => Lb10Large.Value.Reduce( ctx.Cnc, mv ) );
        }

        // from WolframAlpha:
        const string LbEString = "1.4426950408889634073599246810018921374266459541529859341354494069311092191811850798855266228935063444969975183096525442555931016871683596427206621582234793362745373698847184936307013876635320155338943189166648376431286154240474784222894979047950915303513385880549688658930969963680361105110756308441454272158283";
        static readonly Lazy<Fraction> LbELarge = new( ( ) => TryParse( LbEString )!.AsApprox( ) );
        static readonly ConcurrentDictionary<BigInteger /* maxVal */, Fraction> LbECached = new( );

        static Fraction GetLbE( CalculationContext ctx )
        {
            return LbECached.GetOrAdd( ctx.MaxVal, mv => LbELarge.Value.Reduce( ctx.Cnc, mv ) );
        }

        // from WolframAlpha:
        const string Ln2String = "0.693147180559945309417232121458176568075500134360255254120680009493393621969694715605863326996418687542001481020570685733685520235758130557032670751635075961930727570828371435190307038623891673471123350115364497955239120475172681574932065155524734139525882950453007095326366642654104239157814952043740430385500801944";
        static readonly Lazy<Fraction> Ln2Large = new( ( ) => TryParse( Ln2String )!.AsApprox( ) );
        static readonly ConcurrentDictionary<BigInteger /* maxVal */, Fraction> Ln2Cached = new( );

        static Fraction GetLn2( CalculationContext ctx )
        {
            return Ln2Cached.GetOrAdd( ctx.MaxVal, mv => Ln2Large.Value.Reduce( ctx.Cnc, mv ) );
        }

        // from WolframAlpha:
        const string Ln10String = "2.302585092994045684017991454684364207601101488628772976033327900967572609677352480235997205089598298341967784042286248633409525465082806756666287369098781689482907208325554680843799894826233198528393505308965377732628846163366222287698219886746543667474404243274365155048934314939391479619404400222105101714174800368808";
        static readonly Lazy<Fraction> Ln10Large = new( ( ) => TryParse( Ln10String )!.AsApprox( ) );
        static readonly ConcurrentDictionary<BigInteger /* maxVal */, Fraction> Ln10Cached = new( );

        static Fraction GetLn10( CalculationContext ctx )
        {
            return Ln10Cached.GetOrAdd( ctx.MaxVal, mv => Ln10Large.Value.Reduce( ctx.Cnc, mv ) );
        }

        // from WolframAlpha:
        const string Sqrt2DivPiString = "0.7978845608028653558798921198687637369517172623298693153318516593413158517986036770025046678146138728606051177252703653710219839091116744859924254612510154126905411654409986351290326916150611945072854641673391869565434059983728381269120656178667772134093073055934337386839095423544241306507507382669575011208481114";
        static readonly Lazy<Fraction> Sqrt2DivPiLarge = new( ( ) => TryParse( Sqrt2DivPiString )!.AsApprox( ) );
        static readonly ConcurrentDictionary<BigInteger /* maxVal */, Fraction> Sqrt2DivPiCached = new( );

        static Fraction GetSqrt2DivPi( CalculationContext ctx )
        {
            return Sqrt2DivPiCached.GetOrAdd( ctx.MaxVal, mv => Sqrt2DivPiLarge.Value.Reduce( ctx.Cnc, mv ) );
        }

        #endregion


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
            However, instead of the mentioned details in Python (https://www.johndcook.com/blog/2010/10/20/best-rational-approximation/),
            this code is based on JavaScript function, which is different and seems to give better results.

            The function was improved for speed.

            The original JavaScript function, extracted from Web page:

            // x -- the floating-point value to approximate, 0 < x < 1; 
            // N -- the largest possible denominator (and nominator)

            function farey(x, N) 
            {
                var n0 = 0;
                var d0 = 1;
                var n1 = 1;
                var d1 = 1;
                var nm, dm, y;

                dm = d0 + d1;
                while (dm <= N) {
                    nm = n0 + n1;
                    y = x * dm;
                    if (y === nm) {
                        // Found an exact match
                        return [nm, dm];
                    } else if (y > nm) {
                        // Mediant is smaller than x (nm/dm < x)
                        n0 = nm;
                        d0 = dm;
                    } else {
                        // Mediant is larger than x
                        n1 = nm;
                        d1 = dm;
                    }
                    dm = d0 + d1;
                }

                // Which result is closest, n0/d0 or n1/d1?
                // x - n0/d0 < n1/d1 - x
                // 2 * x < n1/d1 + n0/d0
                // 2 * x * d0 * d1 < n1 * d0 + n0 * d1
                if ((2 * x * d0 * d1) > (n1 * d0 + n0 * d1)) {
                    return [n1, d1];
                }
                return [n0, d0];
            }
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
        /// </summary>
        /// <param name="cnc"></param>
        /// <param name="maxVal"></param>
        /// <returns></returns>
        public Fraction Reduce( ICancellable cnc, BigInteger maxVal, bool noE = false )
        {
            if( !IsNormal ) return this;
            if( IsZero ) return this;
            if( QuickTestOne ) return One.UnionApprox( IsApprox );
            if( QuickTestMinusOne ) return MinusOne.UnionApprox( IsApprox );

            if( noE && ( this.CompareTo( cnc, MinusOne ) > 0 && this.CompareTo( cnc, One ) < 0 ) )
            {
                // 'this' is ]-1...+1[

                if( E > 1000 ) throw new ApplicationException( "Too large E." ); //........

                BigInteger n1 = BigInteger.Abs( N );
                BigInteger d1 = D;
                BigInteger e1 = E;

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
        /// Reduce large nominator and denominator, and adjust exponent. (Find the best approximation).
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
        /// “1/value”.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Inv( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsAnyInfinity ) return Zero;
            if( value.IsZero ) return Undefined;

            Debug.Assert( value.IsNormal );

            return new Fraction( value.N < 0 ? -value.D : value.D, value.N < 0 ? -value.N : value.N, -value.E, isApprox: value.IsApprox, isSimplified: value.IsSimplified );
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
        /// Quotient (integer division).
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Quotient( Fraction left, Fraction right, CalculationContext ctx )
        {
            /*
                      Div Rem
             7 #  3:   2,  1
             7 # -3:  -2,  1
            -7 #  3:  -2, -1
            -7 # -3:   2, -1

             7 #  7:   1,  0
             7 # -7:  -1,  0
            -7 #  7:  -1,  0
            -7 # -7:   1,  0

             3 #  7:   0,  3
             3 # -7:   0,  3
            -3 #  7:   0, -3
            -3 # -7:   0, -3              
             */

            if( !left.IsNormal ) return Undefined;
            if( right.IsZero ) return Undefined;

            Fraction this_abs = Abs( left, ctx );
            Fraction y_abs = Abs( right, ctx );

            switch( this_abs.CompareTo( ctx.Cnc, y_abs ) )
            {
            case < 0: // x < y
                return Zero.UnionApprox( left, right );
            case 0: // x == y
            {
                Fraction r = One.UnionApprox( left, right );
                if( left.IsNegative != right.IsNegative ) r = Neg( r, ctx );

                return r;
            }
            }

            // x > y

            BigInteger n = this_abs.N * y_abs.D;
            BigInteger d = this_abs.D * y_abs.N;
            BigInteger e = this_abs.E - y_abs.E;

            while( e < 0 )
            {
                d *= Bi10;

                if( n < d ) return left.UnionApprox( right ); // ex.: 7e-10/3

                ++e;

                ctx.Cnc.TryThrow( );
            }

            Debug.Assert( e >= 0 );

            if( e == 0 && n < d ) return left.UnionApprox( right ); // ex.: 3/4

            BigInteger result = BigInteger.Zero;

            for(; ; )
            {
                ctx.Cnc.TryThrow( );

                Debug.Assert( e >= 0 );
                Debug.Assert( n > 0 );

                (BigInteger q, BigInteger r) = BigInteger.DivRem( n, d );

                result = result * Bi10 + q;

                if( r.IsZero || e.IsZero )
                {
                    return new Fraction( left.IsNegative != right.IsNegative ? -result : result, BigInteger.One, e, isApprox: left.IsApprox, isSimplified: false ).Reduce( ctx );
                }

                if( result >= ctx.MaxVal )
                {
                    Debug.Assert( e > 0 );

                    // get one more digit and round

                    n = r * Bi10;

                    (q, BigInteger _) = BigInteger.DivRem( n, d );

                    Debug.Assert( q >= 0 && q <= 9 );

                    if( q >= Bi5 ) ++result;

                    return new Fraction( left.IsNegative != right.IsNegative ? -result : result, BigInteger.One, e, isApprox: true, isSimplified: false ).Reduce( ctx );
                }

                n = r * Bi10;
                --e;
            }
        }

        /// <summary>
        /// Remainder.
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Remainder( Fraction left, Fraction right, CalculationContext ctx )
        {
            if( !left.IsNormal ) return Undefined;
            if( right.IsZero ) return Undefined;

            Fraction this_abs = Abs( left, ctx );
            Fraction y_abs = Abs( right, ctx );

            switch( this_abs.CompareTo( ctx.Cnc, y_abs ) )
            {
            case < 0: // x < y
                return left.UnionApprox( right );
            case 0: // x == y
                return Zero.UnionApprox( left, right );
            }

            var a = this_abs.TrimZeroes( ctx.Cnc ).ToNDE( );
            var b = y_abs.TrimZeroes( ctx.Cnc ).ToNDE( );

            BigInteger e = -BigInteger.Min( a.e, b.e );

            a.e += e;
            b.e += e;

            Debug.Assert( a.e >= 0 );

            var a2 = a;
            (BigInteger n, BigInteger d) b2 = FractionUtilities.NDEtoND( b );
            (BigInteger n, BigInteger d) r = (0, 0);

            const int se = 300;
            BigInteger s = BigInteger.Pow( Bi10, se );

            for(; ; )
            {
                //ctx.Cnc.TryThrow( );

                (BigInteger n, BigInteger d) t = (a2.n * b2.d, a2.d * b2.n);
                (BigInteger q, BigInteger _) = BigInteger.DivRem( t.n, t.d );

                r = (a2.n * b2.d - q * b2.n * a2.d, a2.d * b2.d);
                BigInteger gcd = FractionUtilities.GreatestCommonDivisor( ctx.Cnc, r.n, r.d );

                r.n /= gcd;
                r.d /= gcd;

                if( a2.e == 0 ) break;

                BigInteger k = BigInteger.Min( a2.e, se );

                a2.e -= k;
                a2.n = r.n * ( k == se ? s : BigInteger.Pow( Bi10, (int)k ) );
                a2.d = r.d;
            }

            return new Fraction( left.N < 0 ? -r.n : r.n, r.d, -e, isApprox: left.IsApprox | right.IsApprox, isSimplified: false ).Reduce( ctx );
        }

        #endregion


        #region Rounding

        /// <summary>
        /// Truncate (toward zero).
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Truncate( Fraction value, CalculationContext ctx )
        {
            if( !value.IsNormal ) return value;
            if( value.IsZero ) return value;

            BigInteger n = value.N < 0 ? -value.N : value.N;
            BigInteger d = value.D;
            BigInteger e = value.E;

            while( e < 0 )
            {
                d *= Bi10;

                if( n < d ) return Zero.UnionApprox( value ); // ex.: 7e-10/3

                ++e;

                ctx.Cnc.TryThrow( );
            }

            Debug.Assert( e >= 0 );

            if( e == 0 && n < d ) return Zero.UnionApprox( value ); // ex.: 3/4

            BigInteger result = BigInteger.Zero;

            for(; ; )
            {
                ctx.Cnc.TryThrow( );

                Debug.Assert( e >= 0 );
                Debug.Assert( n > 0 );

                (BigInteger q, BigInteger r) = BigInteger.DivRem( n, d );

                result = result * Bi10 + q;

                if( r.IsZero || e.IsZero ) break;

                if( result >= ctx.MaxVal )
                {
                    Debug.Assert( e > 0 );

                    // get one more digit and round

                    n = r * Bi10;

                    (q, BigInteger _) = BigInteger.DivRem( n, d );

                    Debug.Assert( q >= 0 && q <= 9 );

                    if( q >= Bi5 ) ++result;

                    break;
                }

                n = r * Bi10;
                --e;
            }

            // make sure the precision is honored

            if( result > ctx.MaxVal )
            {
                BigInteger r;

                do
                {
                    (result, r) = BigInteger.DivRem( result, Bi10 );
                    ++e;

                } while( result > ctx.MaxVal );

                // round
                if( r >= Bi5 ) ++result;
            }

            if( result > ctx.MaxVal ) // when 999 becomes 1000 after previous rounding
            {
                result = BigInteger.Divide( result, Bi10 );
                ++e;
            }

            return new Fraction( value.N < 0 ? -result : result, BigInteger.One, e, isApprox: true, isSimplified: false ); // no 'Reduce'
        }

        /// <summary>
        /// Floor.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Floor( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return NegativeInfinity;
            if( value.IsPositiveInfinity ) return PositiveInfinity;

            if( value.IsPositiveOrZero )
            {
                return Truncate( value, ctx );
            }
            else
            {
                // TODO: reconsider

                Fraction r = Truncate( value, ctx );

                if( !r.Equals( ctx.Cnc, value ) )
                {
                    r = Sub( r, One, ctx );
                }

                return r;
            }
        }

        /// <summary>
        /// Ceiling.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Ceiling( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return NegativeInfinity;
            if( value.IsPositiveInfinity ) return PositiveInfinity;

            if( value.IsPositiveOrZero )
            {
                // TODO: reconsider

                Fraction r = Truncate( value, ctx );

                if( !r.Equals( ctx.Cnc, value ) )
                {
                    r = Add( r, One, ctx );
                }

                return r;
            }
            else
            {
                return Truncate( value, ctx );
            }
        }

        /// <summary>
        /// Round (away from zero).
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Round( Fraction value, CalculationContext ctx ) // "Away from zero"
        {
            if( !value.IsNormal ) return value;
            if( value.IsZero ) return value;

            // TODO: optimize

            Fraction t = Truncate( value, ctx );
            Fraction s = Sub( value, t, ctx );

            if( s.IsNegative )
            {
                if( s.CompareTo( ctx.Cnc, MinusHalf ) <= 0 )
                {
                    return Add( t, MinusOne, ctx );
                }
                else
                {
                    return t;
                }
            }
            else
            {
                if( s.CompareTo( ctx.Cnc, Half ) >= 0 )
                {
                    return Add( t, One, ctx );
                }
                else
                {
                    return t;
                }
            }
        }

        #endregion


        #region Exponentiation

        /// <summary>
        /// Power of x to y.
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Pow( Fraction left, Fraction right, CalculationContext ctx )
        {
            if( left.IsUndefined ) return Undefined;
            if( right.IsUndefined ) return Undefined;

            if( left.IsNegativeInfinity )
            {
                if( right.IsNegativeInfinity ) return Zero;
                if( right.IsNegative ) return Zero;
                if( right.IsPositiveOrZero )
                {
                    switch( right.CompareTo( ctx.Cnc, One ) )
                    {
                    case < 0: // 0<= y < 1
                        return Undefined;
                    default: // y >= 1
                        return NegativeInfinity;
                    }
                }
                if( right.IsPositiveInfinity ) return Undefined; // actually "complex infinity"
                Debug.Assert( false );
            }

            if( left.IsZero ) // 0**y
            {
                if( right.IsNegativeInfinity ) return Undefined;
                if( right.IsPositiveInfinity ) return Zero.UnionApprox( left );

                Debug.Assert( right.IsNormal );

                if( right.IsNegativeOrZero ) return Undefined;

                return Zero.UnionApprox( left ); // > 0
            }

            if( left.IsOne( ctx.Cnc ) ) // 1**y
            {
                if( right.IsNegativeInfinity ) return Undefined;
                if( right.IsPositiveInfinity ) return Undefined;

                Debug.Assert( right.IsNormal );

                return One.UnionApprox( left, right );
            }

            if( left.IsPositiveInfinity )
            {
                if( right.IsNegativeInfinity ) return Zero;
                if( right.IsNegative ) return Zero; // (no approx)
                if( right.IsZero ) return Undefined;
                if( right.IsPositiveNonZero ) return PositiveInfinity;
                if( right.IsPositiveInfinity ) return Undefined; // actually "complex infinity"
            }

            Debug.Assert( left.IsNormal && !left.IsZero ); // (and not "1")
            Debug.Assert( !right.IsUndefined );

            if( right.IsNegativeInfinity )
            {
                switch( left.CompareTo( ctx.Cnc, MinusOne ) )
                {
                case < 0: // x < -1
                    return Zero; // (no approx)
                default:
                    if( left.IsNegativeOrZero ) return Undefined; // x = [-1..0]

                    switch( left.CompareTo( ctx.Cnc, One ) )
                    {
                    case < 0: // x = ]0, 1[
                        return PositiveInfinity;
                    case 0: // x == 1
                        return Undefined;
                    default: // x > 1
                        return Zero; // (no approx)
                    }
                }
            }
            if( right.IsZero ) // x**0
            {
                Debug.Assert( !left.IsZero );

                return One.UnionApprox( left, right );
            }
            if( right.IsPositiveInfinity )
            {
                if( left.CompareTo( ctx.Cnc, MinusOne ) <= 0 ) return Undefined; // x <= -1
                switch( left.CompareTo( ctx.Cnc, One ) )
                {
                case < 0: // -1 < x < 1
                    return Zero; // (no approx)
                case 0: // x = 1
                    return Undefined;
                default: // x > 1
                    return PositiveInfinity;
                }
            }

            Debug.Assert( right.IsNormal );
            Debug.Assert( !right.IsZero );

            if( left.Equals( ctx.Cnc, Two ) ) return PowerOfTwo( right, ctx ).UnionApprox( left );
            if( left.Equals( ctx.Cnc, GetE( ctx ) ) ) return Exp( right, ctx ).UnionApprox( left );
            if( right.Equals( ctx.Cnc, Half ) && GetSqrtMode( ctx ) != SqrtModeEnum.Pow ) return Sqrt( left, ctx ).UnionApprox( right );
            if( right.IsMinusOne( ctx.Cnc ) ) return Inv( left, ctx ).UnionApprox( right );

            right = right.Simplify( ctx );

            if( right.D == 1 && right.E >= 0 ) // integer power
            {
                if( left.Equals( ctx.Cnc, new Fraction( -1 ) ) ) // -1 to integer power
                {
                    if( right.E != 0 || right.N.IsEven ) // -1 to integer even power
                    {
                        return One.UnionApprox( left, right );
                    }
                    else
                    {
                        // -1 to integer odd power

                        return left.UnionApprox( right );
                    }
                }

                bool y_is_negative;

                if( right.IsNegative )
                {
                    y_is_negative = true;
                    right = Neg( right, ctx );
                }
                else
                {
                    y_is_negative = false;
                }

                Fraction p1 = IntPow( left, right.N, ctx );
                BigInteger p2 = BigInteger.Pow( Bi10, (int)right.E );
                Fraction r = IntPow( p1, p2, ctx );

                if( y_is_negative ) r = Inv( r, ctx );

                return r;
            }

            // x**y = e**(y*ln(x))

            Fraction result = Exp( Mul( Ln( left, ctx ), right, ctx ), ctx );

            // try determining if result is exact

            if( result.IsApprox && !left.IsApprox && !right.IsApprox )
            {
                try
                {
                    Fraction right_s = right.Simplify( ctx );

                    if( right_s.E.IsZero )
                    {
                        Fraction left_pow_n = IntPow( left, right_s.N, ctx );

                        if( !left_pow_n.IsApprox )
                        {
                            Fraction result_nonApprox = result.AsNonApprox( );
                            Fraction right_pow_d = IntPow( result_nonApprox, right_s.D, ctx );

                            if( !right_pow_d.IsApprox )
                            {
                                if( left_pow_n.Equals( ctx.Cnc, right_pow_d ) )
                                {
                                    result = result_nonApprox;
                                }
                            }
                        }
                    }
                }
                catch( Exception exc )
                {
                    _ = exc;
                    if( Debugger.IsAttached ) Debugger.Break( );
                }
            }

            return result;
        }

        internal static Fraction PowerOfTwo( Fraction value, CalculationContext ctx ) // 2**x
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Zero;
            if( value.IsPositiveInfinity ) return PositiveInfinity;
            if( value.IsZero ) return One.UnionApprox( value );

            Debug.Assert( value.IsNormal );

            Fraction x = ( value.IsNegative ? Neg( value, ctx ) : value ).Simplify( ctx );
            Fraction t = Truncate( x, ctx ).Simplify( ctx );
            Fraction diff = Sub( x, t, ctx );

            // 2**t, t is integer

            Debug.Assert( t.D.IsOne );
            Debug.Assert( diff <= One );
            Debug.Assert( diff.IsPositiveOrZero );

            Fraction r = IntPow( Two, t.N, ctx );

            // to 10**E

            (BigInteger q, BigInteger r) ee = BigInteger.DivRem( t.E, BucketSize );
            // TODO: if too big return +/- infinity (?)
            for( int i = 0; i < (int)ee.q; ++i )
            {
                r = IntPow( r, TenPowBucket, ctx );
            }
            if( !ee.r.IsZero ) r = IntPow( r, BigInteger.Pow( Bi10, checked((int)ee.r) ), ctx );

            if( !diff.IsZero )
            {
                // WA: 2^x = sum_(n=0)^∞ (x^n log^n(2))/(n!)
                // when n=0: 1
                // when n=1: x * log(2)

                Fraction ln_2 = GetLn2( ctx );

                Fraction current = new( diff.N * ln_2.N, diff.D * ln_2.D, ln_2.E + diff.E );  // when n=1
                Fraction sum = Add( One, current, ctx );
                Fraction eps = new( BigInteger.One, ctx.MaxVal, BigInteger.MinusOne );

                for( int i = 2; ; ++i )
                {
                    Fraction next = new Fraction( current.N * diff.N * ln_2.N, current.D * diff.D * ln_2.D * i, current.E + diff.E ).Reduce( ctx );

                    if( next.CompareTo( ctx.Cnc, eps ) < 0 ) break;

                    sum = Add( sum, next, ctx );
                    current = next;
                }

                r = Mul( r, sum, ctx );
            }

            return ( value.IsNegative ? Inv( r, ctx ) : r ).UnionApprox( value );
        }

        internal enum SqrtModeEnum
        {
            Heron = 1,
            Pow = 2,
            DFloatHeron = 3,
            DFloatBinarySearch = 4,
        }

        static SqrtModeEnum GetSqrtMode( CalculationContext ctx )
        {
            return SqrtModeEnum.DFloatHeron;
        }

        /// <summary>
        /// Square root.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <exception cref="ApplicationException"></exception>
        public static Fraction Sqrt( Fraction value, CalculationContext ctx )
        {
            SqrtModeEnum mode = GetSqrtMode( ctx );

            if( value.IsUndefined ) return Undefined;
            if( value.IsPositiveInfinity ) return PositiveInfinity;
            if( value.IsNegativeInfinity ) return Undefined;
            Debug.Assert( value.IsNormal );
            if( value.IsNegative ) return Undefined;
            if( value.IsZero ) return value;

            switch( mode )
            {
            case SqrtModeEnum.Heron:
            {
                Fraction s = value.Simplify( ctx ).TrimZeroes( ctx.Cnc );
                Fraction eps = new( BigInteger.One, ctx.MaxVal, BigInteger.MinusOne );

                BigInteger n, d, e;

                if( !s.E.IsEven )
                {
                    n = s.N;
                    d = s.D * 10;
                    e = s.E + 1;
                }
                else
                {
                    n = s.N;
                    d = s.D;
                    e = s.E;
                }

                // sqrt(a) using Heron's method (Babylonian method).

                Fraction current = One;

                for(; ; )
                {
                    Fraction next = new Fraction( current.N * current.N * d + n * current.D * current.D, current.D * d * current.N * 2 ).Reduce( ctx );
                    if( Abs( Sub( next, current, ctx ), ctx ).CompareTo( ctx.Cnc, eps ) < 0 ) break;

                    var t = FractionUtilities.NDEtoND( next.ToNDE( ) );
                    current = new Fraction( t.n, t.d ).UnionApprox( next );
                }

                Debug.Assert( e.IsEven );

                Fraction result = new( current.N, current.D, current.E + e / 2, isApprox: value.IsApprox || current.IsApprox, isSimplified: false );

                return result;
            }
            case SqrtModeEnum.Pow:
            {
                return Pow( value, Half, ctx );
            }
            case SqrtModeEnum.DFloatHeron:
            {
                Fraction x = value.Simplify( ctx );
                DFloat f = x.ToDFloat( ctx.MaxBytes * 2 );
                DFloat y = DFloat.SqrtHeron( f, ctx.MaxBytes * 2 );
                Fraction r = FromDFloat( y ).Reduce( ctx ).UnionApprox( value );

                // check if result is exact, by multiplication
                if( !value.IsApprox && r.IsApprox )
                {
                    Fraction rna = r.AsNonApprox( );
                    Fraction p = Mul( rna, rna, ctx );

                    if( !p.IsApprox && p.Equals( ctx.Cnc, value ) ) r = rna;
                }

                return r;
            }
            case SqrtModeEnum.DFloatBinarySearch:
            {
                Fraction x = value.Simplify( ctx );
                DFloat f = x.ToDFloat( ctx.MaxBytes * 2 );
                DFloat y = DFloat.SqrtBinarySearch( f, ctx.MaxBytes * 2 );
                Fraction r = FromDFloat( y ).Reduce( ctx );

                return r;
            }
            default:
                throw new ApplicationException( "Invalid mode" );
            }
        }

        enum ExpModeEnum
        {
            DFloat = 1,
            PowLb = 2,
            Series = 3,
        }

        /// <summary>
        /// Natural exponent.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <exception cref="ApplicationException"></exception>
        public static Fraction Exp( Fraction value, CalculationContext ctx )
        {
            ExpModeEnum mode = ExpModeEnum.DFloat;

            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Zero;
            if( value.IsPositiveInfinity ) return PositiveInfinity;
            if( value.IsZero ) return One.UnionApprox( value );
            if( value.QuickTestOne ) return GetE( ctx ); // (IsApprox)
            if( value.QuickTestMinusOne ) return Inv( GetE( ctx ), ctx ); // (IsApprox)

            Debug.Assert( value.IsNormal );

            switch( mode )
            {
            case ExpModeEnum.DFloat:
            {
                Fraction x = value.Simplify( ctx );

                int mb = ctx.MaxBytes * 2;

                DFloat f = x.ToDFloat( mb );
                DFloat y = DFloat.Exp( f, mb );

                Fraction r = FromDFloat( y ).Reduce( ctx );

                return r;
            }
            case ExpModeEnum.PowLb:
            case ExpModeEnum.Series:
            {
                Fraction x = ( value.IsNegative ? Neg( value, ctx ) : value ).Simplify( ctx );
                Fraction r = ExpPositive( x, ctx, mode ).UnionApprox( x );

                return value.IsNegative ? Inv( r, ctx ) : r;
            }
            default:
                throw new ApplicationException( "Invalid mode" );
            }
        }

        static Fraction ExpPositive( Fraction value, CalculationContext ctx, ExpModeEnum mode )
        {
            Debug.Assert( value.IsSimplified );

            if( value.D == 1 && value.E >= 0 ) // e^int
            {
                Fraction number_e = GetE( ctx );
                Fraction r = IntPow( number_e, value.N, ctx );

                (BigInteger q, BigInteger r) ee = BigInteger.DivRem( value.E, BucketSize );
                // TODO: if too big return +/- infinity (?)
                for( int i = 0; i < (int)ee.q; ++i )
                {
                    r = IntPow( r, TenPowBucket, ctx );
                }
                if( !ee.r.IsZero ) r = IntPow( r, BigInteger.Pow( Bi10, checked((int)ee.r) ), ctx );

                return r;
            }

            switch( mode )
            {
            case ExpModeEnum.PowLb:
            {
                // TODO: remove; it is slower and less precise

                // e^x = 2^(x*lb(e))

                Fraction p = Mul( value, GetLbE( ctx ), ctx );
                p = PowerOfTwo( p, ctx );

                return p;
            }
            case ExpModeEnum.Series:
            {
                Fraction t = Truncate( value, ctx ).Simplify( ctx );
                Fraction diff = Sub( value, t, ctx );

                // e**t, t is integer

                Debug.Assert( t.D.IsOne );
                Debug.Assert( diff <= One );
                Debug.Assert( diff.IsPositiveOrZero );

                Fraction r = IntPow( GetE( ctx ), t.N, ctx );

                // to 10**E

                (BigInteger q, BigInteger r) ee = BigInteger.DivRem( t.E, BucketSize );
                // TODO: if too big return +/- infinity (?)
                for( int i = 0; i < (int)ee.q; ++i )
                {
                    r = IntPow( r, TenPowBucket, ctx );
                }
                if( !ee.r.IsZero ) r = IntPow( r, BigInteger.Pow( Bi10, checked((int)ee.r) ), ctx );

                if( !diff.IsZero )
                {
                    // WA: e^x = sum_(k=0)^∞ x^k/(k!)

                    Fraction current = diff;
                    Fraction sum = current;
                    Fraction eps = new( BigInteger.One, ctx.MaxVal, -1 );

                    for( int i = 2; ; ++i )
                    {
                        var next = new Fraction( current.N * diff.N, current.D * diff.D * i, current.E + diff.E ).Reduce( ctx );

                        if( next.CompareTo( ctx.Cnc, eps ) < 0 ) break;

                        sum = Add( sum, next, ctx );
                        current = next;
                    }

                    sum = Add( sum, One, ctx );
                    r = Mul( r, sum, ctx );
                }

                return r;
            }
            default:
                throw new ArgumentOutOfRangeException( nameof( mode ) );
            }
        }

        internal static Fraction IntPow( Fraction left, BigInteger right, CalculationContext ctx )
        {
            // TODO: detect very large p

            if( left.IsUndefined ) return Undefined;

            if( left.IsZero )
            {
                if( right > 0 ) return Zero.UnionApprox( left );
                if( right < 0 ) return Undefined;

                Debug.Assert( right == 0 );

                return Undefined; // (or 1 in some theories)
            }

            Debug.Assert( left.IsNormal );
            Debug.Assert( !left.IsZero );

            if( right.IsZero ) return One.UnionApprox( left );
            if( left.QuickTestOne ) return One.UnionApprox( left );

            Fraction r = IntPowPositive( left, BigInteger.Abs( right ), ctx );

            return right < 0 ? Inv( r, ctx ) : r;
        }

        static Fraction IntPowPositive( Fraction left, BigInteger right, CalculationContext ctx )
        {
            Debug.Assert( !left.IsZero );
            Debug.Assert( right > 0 );

            if( right.IsOne ) return left; // x**1

            if( left.QuickTestOne ) return left; // 1**y
            if( right.IsZero ) return One.UnionApprox( left ); // x**0

            long bit_length = right.GetBitLength( );
            byte[] bytes = right.ToByteArray( );

            // TODO: reconsider the parallel version, which seemed slower

            Fraction a = Abs( left, ctx );
            Fraction result = One;

            for( long i = 0; i < bit_length; ++i )
            {
                if( ( bytes[i / 8] & (byte)( 1 << (int)( i % 8 ) ) ) != 0 )
                {
                    result = Mul( result, a, ctx );
                }

                a = Mul( a, a, ctx );
            }

            if( left.IsNegative && !right.IsEven ) result = Neg( result, ctx );

            return result.UnionApprox( left );
        }

        #endregion


        #region Logarithm

        enum LbModeEnum
        {
            LbOfComponents = 1,
            Ln = 2,
            DFloat = 3,
        }

        /// <summary>
        /// Binary logarithm (base 2).
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <exception cref="QuasiCalcException"></exception>
        /// <exception cref="ApplicationException"></exception>
        public static Fraction Lb( Fraction value, CalculationContext ctx )
        {
            LbModeEnum mode = LbModeEnum.DFloat;

            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Undefined; // (or +Infinity in some theories)
            if( value.IsNegative ) return Undefined;
            if( value.IsZero ) return Undefined; // (or -Infinity in some theories)
            if( value.IsPositiveInfinity ) return PositiveInfinity;
            if( value.IsOne( ctx.Cnc ) ) return Zero.UnionApprox( value );
            if( value.Equals( ctx.Cnc, Two ) ) return One.UnionApprox( value );

            Debug.Assert( value.IsNormal );
            Debug.Assert( value.IsPositiveNonZero );

            Fraction s = value.Simplify( ctx );

            if( s.D == 1 && s.E == 0 )
            {
                // integer and no E; check if it is power of 2

                if( s.N.IsPowerOfTwo )
                {
                    return new Fraction( BigInteger.Log2( s.N ), BigInteger.One, BigInteger.Zero, isApprox: s.IsApprox, isSimplified: false );
                }
            }

            switch( mode )
            {
            case LbModeEnum.LbOfComponents:  // lb(n*10**e / d) = lb(n) - lb(d) + e*lb(10)
            {
                Fraction lb_n = LbInternal( s.N, ctx );
                Fraction lb_d = LbInternal( s.D, ctx );
                Fraction e = new Fraction( s.E );

                Fraction r = Sub( lb_n, lb_d, ctx );
                if( !e.IsZero ) e = Mul( e, GetLb10( ctx ), ctx ); // else keep 'Approx=false'
                r = Add( r, e, ctx );

                return r;
            }
            case LbModeEnum.Ln:
            {
                if( GetLnMode( ctx ) == LnModeEnum.Lb ) throw new ApplicationException( "Incompatible values of $LB_MODE and $LN_MODE causing infinite recursion." );

                // faster: lb(x) = ln(x) / ln(2)

                Fraction ln = Ln( s, ctx );
                Fraction r = Div( ln, GetLn2( ctx ), ctx );

                return r;
            }
            case LbModeEnum.DFloat:
            {
                int mb = ctx.MaxBytes * 2;

                DFloat t = value.ToDFloat( mb );
                DFloat lb = DFloat.Lb( t, mb );

                Fraction r = FromDFloat( lb ).Reduce( ctx );

                return r;
            }
            default:
                throw new ApplicationException( "Invalid mode" );
            }
        }

        static Fraction LbInternal( BigInteger value, CalculationContext ctx )
        {
            Debug.Assert( value > 0 );

            if( value.IsOne ) return Zero;
            if( value.IsPowerOfTwo ) return new Fraction( BigInteger.Log2( value ), BigInteger.One, BigInteger.Zero, isApprox: false, isSimplified: true );

            // See: https://en.wikipedia.org/wiki/Binary_logarithm

            BigInteger n = BigInteger.Log2( value );

            Fraction y = new Fraction( value, BigInteger.Pow( 2, (int)n ) ).Simplify( ctx );
            Fraction r = new( n );
            Fraction z = new( y );
            int sum_m = 0;
            BigInteger last_2_pow_sum_m = 0;
            BigInteger ten_pow_maxDigits = ctx.MaxVal;
            Fraction eps = new( BigInteger.One, ctx.MaxVal, BigInteger.MinusOne );

            //long dbg_total_iterations = 0;

            for( int k = 0; ; ++k )
            {
                //if( z.IsOne( ctx.Cnc ) ) break;

                Debug.Assert( z.IsPositiveOrZero );

                if( z.CompareTo( ctx.Cnc, eps ) < 0 ) break;

                for( int i = 1; ; ++i )
                {
                    //++dbg_total_iterations;

                    z = Mul( z, z, ctx );

                    if( z.IsOne( ctx.Cnc ) ) break; // out of precision

                    if( z.CompareTo( ctx.Cnc, Two ) >= 0 )
                    {
                        sum_m += i;
                        last_2_pow_sum_m = BigInteger.Pow( 2, sum_m );
                        r = Add( r, new Fraction( 1, last_2_pow_sum_m ), ctx );

                        break;
                    }
                }

                if( z.IsOne( ctx.Cnc ) ) break; // out of precision

                //....
                if( last_2_pow_sum_m > ten_pow_maxDigits )
                {
                    r = r.UnionApprox( true );

                    break;
                }

                z = Mul( z, Half, ctx );
            }

            //Debug.WriteLine( $"*** LbInternal({x}) -- {dbg_total_iterations:#,##0} iterations" );

            return r;
        }

        enum LnModeEnum
        {
            Series = 1,
            Lb = 2,
            DFloat = 3,
            DFloatSub = 4,
        }

        static LnModeEnum GetLnMode( CalculationContext ctx )
        {
            return LnModeEnum.DFloat;
        }

        /// <summary>
        /// Natural logarithm (base e).
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <exception cref="ApplicationException"></exception>
        public static Fraction Ln( Fraction value, CalculationContext ctx )
        {
            LnModeEnum mode = GetLnMode( ctx );

            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Undefined; // (or +Infinity in some theories)
            if( value.IsNegative ) return Undefined;
            if( value.IsZero ) return Undefined; // (or -Infinity in some theories)
            if( value.IsPositiveInfinity ) return PositiveInfinity;
            if( value.IsOne( ctx.Cnc ) ) return Zero.UnionApprox( value );

            Debug.Assert( value.IsNormal );
            Debug.Assert( value.IsPositiveNonZero );

            // TODO: detect ln(e) etc.

            switch( mode )
            {
            case LnModeEnum.Series: // ln using "logarithm series"
            {
                Fraction s = value.Simplify( ctx );
                var n = s.N;
                var d = s.D;
                var e = s.E;

                // make it [1...2[
                while( n < d ) { n *= Bi10; --e; }
                while( n >= 2 * d ) { d *= Bi10; ++e; }

                Fraction x = new( n, d );

                Debug.Assert( x < Two );
                Debug.Assert( x > Zero );
                Debug.Assert( new Fraction( n, d, e ) == s );

                // WA "logarithm series": log(x) = - sum_(k=1)^∞ ((-1)^k (-1 + x)^k)/k for abs(-1 + x)<1

                Debug.Assert( Abs( Add( x, MinusOne, ctx ), ctx ) < One );

                Fraction x_minus_one = new( x.N - x.D, x.D );
                Fraction current = new( -x_minus_one.N, x_minus_one.D );
                Fraction eps = new( BigInteger.One, ctx.MaxVal, BigInteger.MinusOne );

                Fraction sum = current;

                Debug.Assert( x_minus_one.E == 0 );

                for( int k = 2; ; ++k )
                {
                    Fraction next = new Fraction( -current.N * x_minus_one.N * ( k - 1 ), current.D * x_minus_one.D * k, current.E ).Reduce( ctx );

                    if( Abs( next, ctx ) < eps ) break;

                    sum = Add( sum, next, ctx );
                    current = next;
                }

                sum = Neg( sum, ctx );
                Fraction r = GetLn10( ctx );
                r = Mul( r, new Fraction( e ), ctx );
                r = Add( r, sum, ctx );

                return r;
            }
            case LnModeEnum.Lb: // ln via lb: "ln(x) = lb(x) / lb(e)"
            {
                Fraction r = Lb( value, ctx );

                return Div( r, GetLbE( ctx ), ctx );
            }
            case LnModeEnum.DFloat: // ln using dfloat
            {
                Fraction s = value.Simplify( ctx );
                int mb = ctx.MaxBytes * 2;
                DFloat t = s.ToDFloat( mb );
                DFloat ln = DFloat.Ln( t, mb );

                Fraction r = FromDFloat( ln ).Reduce( ctx );

                return r;
            }
            case LnModeEnum.DFloatSub: // ln using dfloat and 'ln(nominator) - ln(denominator)'
            {
                Fraction s = value.Simplify( ctx );
                int mb = ctx.MaxBytes * 2;
                DFloat n = new( s.N, s.E );
                DFloat d = new( s.D );

                DFloat ln_n = DFloat.Ln( n, mb );
                DFloat ln_d = DFloat.Ln( d, mb );
                DFloat ln = DFloat.Sub( ln_n, ln_d, mb );

                Fraction r = FromDFloat( ln ).Reduce( ctx );

                return r;
            }
            default:
                throw new ApplicationException( "Invalid mode" );
            }
        }

        enum LgModeEnum
        {
            Lb = 1,
            Ln = 2,
            DFloat = 3,
        }

        /// <summary>
        /// Decimal logarithm (base 10).
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <exception cref="ApplicationException"></exception>
        public static Fraction Lg( Fraction value, CalculationContext ctx )
        {
            LgModeEnum mode = LgModeEnum.DFloat;

            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Undefined; // (or +Infinity in some theories)
            if( value.IsNegative ) return Undefined;
            if( value.IsZero ) return Undefined; // (or -Infinity in some theories)
            if( value.IsPositiveInfinity ) return PositiveInfinity;
            if( value.IsOne( ctx.Cnc ) ) return Zero.UnionApprox( value );

            Debug.Assert( value.IsNormal );
            Debug.Assert( value.IsPositiveNonZero );

            (BigInteger x, int e) tn = FractionUtilities.TrimZeroesGE0( ctx.Cnc, value.N );
            (BigInteger x, int e) td = FractionUtilities.TrimZeroesGE0( ctx.Cnc, value.D );
            BigInteger e = value.E + tn.e - td.e;

            if( tn.x == td.x ) // 1e±XXX
            {
                return new Fraction( e, BigInteger.One, BigInteger.Zero, isApprox: value.IsApprox, isSimplified: true );
            }

            Fraction t = new Fraction( tn.x, td.x ).UnionApprox( value );

            switch( mode )
            {
            case LgModeEnum.Lb:
            {
                Fraction r = Add( Div( Lb( t, ctx ), GetLb10( ctx ), ctx ), new Fraction( e ), ctx );

                return r;
            }
            case LgModeEnum.Ln:
            {
                Fraction r = Add( Div( Ln( t, ctx ), GetLn10( ctx ), ctx ), new Fraction( e ), ctx );

                return r;
            }
            case LgModeEnum.DFloat:
            {
                int mb = ctx.MaxBytes * 2;

                DFloat f = value.ToDFloat( mb );
                DFloat lg = DFloat.Lg( f, mb );

                Fraction r = FromDFloat( lg ).Reduce( ctx );

                return r;
            }
            default:
                throw new ApplicationException( "Invalid mode" );
            }
        }

        enum LogModeEnum
        {
            Lb = 1,
            DFloat = 2,
        }

        /// <summary>
        /// Logarithm with any base.
        /// </summary>
        /// <param name="base"></param>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <exception cref="ApplicationException"></exception>
        public static Fraction Log( Fraction @base, Fraction value, CalculationContext ctx )
        {
            LogModeEnum mode = LogModeEnum.DFloat;

            // The base of logarithm must be positive and not equal to 1;
            if( !@base.IsPositiveNonZero || @base.IsOne( ctx.Cnc ) ) return Undefined;

            if( value.IsPositiveInfinity ) return PositiveInfinity;
            // The argument of logarithm must be positive
            if( !value.IsPositiveNonZero ) return Undefined;

            if( @base.Equals( ctx.Cnc, Two ) ) return Lb( value, ctx );
            if( @base.Equals( ctx.Cnc, Ten ) ) return Lg( value, ctx );
            if( @base.Equals( ctx.Cnc, GetE( ctx ) ) ) return Ln( value, ctx ); //

            Fraction result;

            switch( mode )
            {
            case LogModeEnum.Lb:
            {
                Fraction lb_x = Lb( value, ctx );
                Fraction lb_base = Lb( @base, ctx );

                result = Div( lb_x, lb_base, ctx );
            }
            break;
            case LogModeEnum.DFloat:
            {
                int mb = ctx.MaxBytes * 2;

                DFloat t = value.ToDFloat( mb );
                DFloat b = @base.ToDFloat( mb );
                DFloat log = DFloat.Log( @base: b, f: t, maxBytes: mb );

                result = FromDFloat( log ).Reduce( ctx );
            }
            break;
            default:
                throw new ApplicationException( "Invalid mode" );
            }

            // try determining if result is exact

            if( result.IsApprox && !@base.IsApprox && !value.IsApprox )
            {
                try
                {
                    Fraction result_nonApprox = result.AsNonApprox( );
                    Fraction p = Pow( @base, result_nonApprox, ctx );

                    if( !p.IsApprox )
                    {
                        if( p.Equals( ctx.Cnc, value ) )
                        {
                            result = result_nonApprox;
                        }
                    }
                }
                catch( Exception exc )
                {
                    _ = exc;
                    if( Debugger.IsAttached ) Debugger.Break( );
                }
            }

            return result;
        }

        #endregion


        #region Factorials and Gamma

        /// <summary>
        /// Factorial.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Factorial( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Undefined;
            if( value.IsPositiveInfinity ) return PositiveInfinity;

            Debug.Assert( value.IsNormal );

            if( value.IsZero || value.QuickTestOne ) return One.UnionApprox( value );

            Fraction s = value.Simplify( ctx ).TrimZeroes( ctx.Cnc );

            if( s.D == 1 && s.E >= 0 )
            {
                // integer

                if( value.IsNegative ) return Undefined; // negative integer

                // positive integer

                DFloat f = DFloat.FactorialOfPositiveInteger( new DFloat( s.N, s.E, s.IsApprox ), ctx.MaxBytes * 2 );
                Fraction r = FromDFloat( f ).Reduce( ctx );

                return r;
            }
            else
            {
                // non-integer positive or negative
                // n! = G(n+1)

                int mb = ctx.MaxBytes * 2;

                DFloat f = DFloat.Add( s.ToDFloat( mb ), DFloat.One, mb );
                DFloat g = DFloat.Gamma( f, mb );
                Fraction r = FromDFloat( g ).Reduce( ctx ).AsApprox( );

                return r;
            }
        }

        /// <summary>
        /// Double factorial.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction DoubleFactorial( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Undefined;
            if( value.IsPositiveInfinity ) return PositiveInfinity;

            Debug.Assert( value.IsNormal );

            if( value.IsZero || value.QuickTestOne || value.QuickTestMinusOne ) return One.UnionApprox( value );

            Fraction s = value.Simplify( ctx ).TrimZeroes( ctx.Cnc );

            if( s.D == 1 && s.E >= 0 )
            {
                // integer

                if( value.IsPositiveOrZero )
                {
                    DFloat f = DFloat.DoubleFactorialOfPositiveInteger( new DFloat( s.N, s.E, s.IsApprox ), ctx.MaxBytes * 2 );
                    Fraction r = FromDFloat( f ).Reduce( ctx );

                    return r;
                }
                else
                {
                    if( s.E > 0 || s.N.IsEven )
                    {
                        // negative even integer

                        return Undefined;
                    }
                    else
                    {
                        // negative odd integer

                        // https://en.wikipedia.org/wiki/Double_factorial, "Negative arguments"
                        // (-n)!! * n!! = (-1)**((n-1)/2) * n ==> n!! = (-1)**((n-1)/2) * n / (-n)!!

                        Debug.Assert( s.E == 0 );

                        int mb = ctx.MaxBytes * 2;

                        DFloat n = s.ToDFloat( mb );
                        DFloat f = DFloat.DoubleFactorialOfPositiveInteger( DFloat.Neg( n ), mb );
                        DFloat d = DFloat.Div( n, f, mb );

                        Fraction r = FromDFloat( d ).Reduce( ctx );

                        if( !( ( s.N - 1 ) / 2 ).IsEven ) r = Neg( r, ctx );

                        return r;
                    }
                }
            }
            else
            {
                // non-integer

                int mb = ctx.MaxBytes + 4;

                DFloat f = s.ToDFloat( mb );
                f = DFloat.DoubleFactorialOfNonInteger( f, mb );
                Fraction r = FromDFloat( f ).Reduce( ctx );

                return r;
            }
        }

        /// <summary>
        /// Gamma function.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Gamma( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Undefined;
            if( value.IsZero ) return Undefined;
            if( value.IsPositiveInfinity ) return PositiveInfinity;
            Debug.Assert( value.IsNormal );
            if( value.IsOne( ctx.Cnc ) ) return One.UnionApprox( value );

            Fraction z = value.Simplify( ctx ).TrimZeroes( ctx.Cnc );

            if( z.D == 1 && z.E >= 0 ) // is integer
            {
                if( z.IsPositiveNonZero )
                {
                    // positive integer; G(z+1) = z!

                    return Factorial( Add( z, MinusOne, ctx ), ctx );
                }
                else
                {
                    // negative integer

                    return Undefined;
                }
            }

            // non-integer positive or negative

            DFloat f = z.ToDFloat( ctx.MaxBytes * 2 );
            DFloat g = DFloat.Gamma( f, ctx.MaxBytes * 2 );

            Fraction r = FromDFloat( g ).Reduce( ctx );

            return r;
        }

        #endregion


        #region Trigonometry

        enum TrigModeEnum
        {
            Series = 1,
            DFloat = 2,
        }

        /// <summary>
        /// Sine.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <exception cref="ApplicationException"></exception>
        public static Fraction Sin( Fraction value, CalculationContext ctx )
        {
            TrigModeEnum mode = TrigModeEnum.DFloat;

            if( !value.IsNormal ) return Undefined;

            switch( mode )
            {
            case TrigModeEnum.Series:
            {
                // WA: x - x^3/6 + x^5/120 + O(x^7) (Taylor series)

                Fraction f = Remainder( value, Get2Pi( ctx ), ctx );

                if( f.IsZero ) return Zero.UnionApprox( value, f );

                if( f.IsApprox )
                {
                    if( Abs( f, ctx ).Equals( ctx.Cnc, GetPi( ctx ) ) ) return Zero.UnionApprox( value, f );
                    if( f.Equals( ctx.Cnc, GetPiOverTwo( ctx ) ) ) return One.UnionApprox( value, f );
                    if( f.Equals( ctx.Cnc, Neg( GetPiOverTwo( ctx ), ctx ) ) ) return MinusOne.UnionApprox( value, f );
                }

                Fraction current = f;
                Fraction sum = current;
                Fraction eps = new( BigInteger.One, ctx.MaxVal, BigInteger.MinusOne );
                bool is_approx = value.IsApprox || f.IsApprox;

                for( int tk = 2; ; tk += 2 )
                {
                    Fraction next = new Fraction( -current.N * f.N * f.N, current.D * f.D * f.D * ( tk * ( tk + 1 ) ), current.E + f.E + f.E ).Reduce( ctx );

                    if( next.IsApprox ) is_approx = true;
                    if( next.IsZero ) break;
                    if( Abs( next, ctx ).CompareTo( ctx.Cnc, eps ) < 0 )
                    {
                        is_approx = true;
                        break;
                    }

                    sum = Add( sum, next, ctx );
                    current = next;
                }

                return sum.UnionApprox( is_approx );
            }
            case TrigModeEnum.DFloat:
            {
                int mb = ctx.MaxBytes * 2;

                DFloat f = value.ToDFloat( mb );
                DFloat r = DFloat.Sin( f, mb );

                return FromDFloat( r ).Reduce( ctx );
            }
            default:
                throw new ApplicationException( "Invalid mode" );
            }
        }

        /// <summary>
        /// Cosine.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <exception cref="ApplicationException"></exception>
        public static Fraction Cos( Fraction value, CalculationContext ctx )
        {
            TrigModeEnum mode = TrigModeEnum.DFloat;

            if( !value.IsNormal ) return Undefined;

            switch( mode )
            {
            case TrigModeEnum.Series:
            {
                // WA: 1 - x^2/2 + x^4/24 - x^6/720 + O(x^7) (Taylor series)

                Fraction f = Remainder( value, Get2Pi( ctx ), ctx );

                if( f.IsZero ) return One.UnionApprox( value, f );

                if( f.IsApprox )
                {
                    if( Abs( f, ctx ).Equals( ctx.Cnc, GetPi( ctx ) ) ) return MinusOne.UnionApprox( value, f );
                    if( Abs( f, ctx ).Equals( ctx.Cnc, GetPiOverTwo( ctx ) ) ) return Zero.UnionApprox( value, f );
                    if( Abs( f, ctx ).Equals( ctx.Cnc, Neg( GetPiOverTwo( ctx ), ctx ) ) ) return Zero.UnionApprox( value, f );
                }

                Fraction current = One;
                Fraction sum = current;
                Fraction eps = new( BigInteger.One, ctx.MaxVal, -1 );
                bool is_approx = false;

                for( int tk = 2; ; tk += 2 )
                {
                    Fraction next = new Fraction( -current.N * f.N * f.N, current.D * f.D * f.D * tk * ( tk - 1 ), current.E + f.E + f.E ).Reduce( ctx );

                    if( next.IsApprox ) is_approx = true;
                    if( next.IsZero ) break;
                    if( Abs( next, ctx ).CompareTo( ctx.Cnc, eps ) < 0 )
                    {
                        is_approx = true;
                        break;
                    }

                    sum = Add( sum, next, ctx );
                    current = next;
                }

                return sum.UnionApprox( value.IsApprox || is_approx );
            }
            case TrigModeEnum.DFloat:
            {
                int mb = ctx.MaxBytes * 2;

                DFloat f = value.ToDFloat( mb );
                DFloat r = DFloat.Cos( f, mb );

                return FromDFloat( r ).Reduce( ctx );
            }
            default:
                throw new ApplicationException( "Invalid mode" );
            }
        }

        /// <summary>
        /// Tangent.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Tg( Fraction value, CalculationContext ctx )
        {
            if( !value.IsNormal ) return Undefined;

            Fraction f = Remainder( value, GetPi( ctx ), ctx );

            if( f.IsZero ) return Zero.UnionApprox( value, f );
            if( value.IsApprox && value.Equals( ctx.Cnc, Div( GetPi( ctx ), Four, ctx ) ) ) return One.UnionApprox( value, f );
            if( value.IsApprox && value.Equals( ctx.Cnc, Neg( Div( GetPi( ctx ), Four, ctx ), ctx ) ) ) return MinusOne.UnionApprox( value, f );
            if( value.IsApprox && Abs( value, ctx ).Equals( ctx.Cnc, Div( GetPi( ctx ), Two, ctx ) ) ) return Undefined;

            int mb = ctx.MaxBytes * 2;

            DFloat df = value.ToDFloat( mb );
            DFloat r = DFloat.Tg( df, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        /// <summary>
        /// Cotangent.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Ctg( Fraction value, CalculationContext ctx )
        {
            if( !value.IsNormal ) return Undefined;

            Fraction f = Remainder( value, GetPi( ctx ), ctx );

            if( f.IsZero ) return Undefined;
            if( value.IsApprox && value.Equals( ctx.Cnc, Div( GetPi( ctx ), Four, ctx ) ) ) return One.UnionApprox( value, f );
            if( value.IsApprox && value.Equals( ctx.Cnc, Neg( Div( GetPi( ctx ), Four, ctx ), ctx ) ) ) return MinusOne.UnionApprox( value, f );
            if( value.IsApprox && Abs( value, ctx ).Equals( ctx.Cnc, Div( GetPi( ctx ), Two, ctx ) ) ) return Zero.UnionApprox( value, f );

            int mb = ctx.MaxBytes * 2;

            DFloat df = value.ToDFloat( mb );
            DFloat r = DFloat.Ctg( df, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        /// <summary>
        /// Secant.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Sec( Fraction value, CalculationContext ctx )
        {
            if( !value.IsNormal ) return Undefined;

            Fraction cos = Cos( value, ctx );
            Fraction r = Inv( cos, ctx );

            return r;
        }

        /// <summary>
        /// Cosecant.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Csc( Fraction value, CalculationContext ctx )
        {
            if( !value.IsNormal ) return Undefined;

            Fraction sin = Sin( value, ctx );
            Fraction r = Inv( sin, ctx );

            return r;
        }

        /// <summary>
        /// Arcsine.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction ArcSin( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsAnyInfinity ) return Undefined;
            if( value.IsZero ) return Zero;

            switch( value.CompareTo( ctx.Cnc, MinusOne ) )
            {
            case < 0: // value < -1
                return Undefined;
            case 0: // value == -1
                return Neg( GetPiOverTwo( ctx ), ctx );
            }

            switch( value.CompareTo( ctx.Cnc, One ) )
            {
            case > 0: // value > 1
                return Undefined;
            case 0: // value == 1
                return GetPiOverTwo( ctx );
            }

            int mb = ctx.MaxBytes * 2;

            DFloat f = value.ToDFloat( mb );
            DFloat r = DFloat.ArcSinViaArcTan( f, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        /// <summary>
        /// Arccosine.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction ArcCos( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsAnyInfinity ) return Undefined;
            if( value.IsZero ) return GetPiOverTwo( ctx );

            switch( value.CompareTo( ctx.Cnc, MinusOne ) )
            {
            case < 0: // value < -1
                return Undefined;
            case 0: // value == -1
                return GetPi( ctx );
            }

            switch( value.CompareTo( ctx.Cnc, One ) )
            {
            case > 0: // value > 1
                return Undefined;
            case 0: // value == 1
                return Zero;
            }

            int mb = ctx.MaxBytes * 2;

            DFloat f = value.ToDFloat( mb );
            DFloat r = DFloat.ArcCosViaArcTan( f, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        /// <summary>
        /// Arctangent.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction ArcTan( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsPositiveInfinity ) return GetPiOverTwo( ctx );
            if( value.IsNegativeInfinity ) return Neg( GetPiOverTwo( ctx ), ctx );
            if( value.IsZero ) return Zero;
            if( value.IsOne( ctx.Cnc ) ) return GetPiOverFour( ctx );
            if( value.IsMinusOne( ctx.Cnc ) ) return Neg( GetPiOverFour( ctx ), ctx );

            Debug.Assert( value.IsNormal );

            int mb = ctx.MaxBytes * 2;

            DFloat f = value.ToDFloat( mb );
            DFloat r = DFloat.ArcTanNewtonAccelerated( f, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        /// <summary>
        /// Arccotangent.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction ArcCot( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsPositiveInfinity ) return Zero;
            if( value.IsNegativeInfinity ) return Zero;
            if( value.IsZero ) return GetPiOverTwo( ctx );
            if( value.IsOne( ctx.Cnc ) ) return GetPiOverFour( ctx );
            if( value.IsMinusOne( ctx.Cnc ) ) return Neg( GetPiOverFour( ctx ), ctx );

            Debug.Assert( value.IsNormal );

            int mb = ctx.MaxBytes * 2;

            DFloat f = value.ToDFloat( mb );
            DFloat r = DFloat.ArcCotNewtonAccelerated( f, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        public static Fraction ArcSec( Fraction value, CalculationContext ctx )
        {
            // https://en.wikipedia.org/wiki/Inverse_trigonometric_functions, "Relationships among the inverse trigonometric functions"
            // arcsec(x) = arccos(1/x)

            Fraction f = Inv( value, ctx );
            Fraction r = ArcCos( f, ctx );

            return r;
        }

        public static Fraction ArcCsc( Fraction value, CalculationContext ctx )
        {
            // https://en.wikipedia.org/wiki/Inverse_trigonometric_functions, "Relationships among the inverse trigonometric functions"
            // arccsc(x) = arcsin(1/x)

            Fraction f = Inv( value, ctx );
            Fraction r = ArcSin( f, ctx );

            return r;
        }

        /// <summary>
        /// Degrees to radians.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction ToRad( Fraction value, CalculationContext ctx )
        {
            if( !value.IsNormal ) return value;

            Fraction f = Remainder( value, new Fraction( 360 ), ctx );
            if( f.IsZero ) return f;
            Fraction pi = GetPi( ctx );

            return new Fraction( f.N * pi.N, f.D * pi.D * 180, f.E + pi.E, isApprox: true, isSimplified: false ).Reduce( ctx );
        }

        /// <summary>
        /// Radians to degrees.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction ToDeg( Fraction value, CalculationContext ctx )
        {
            if( !value.IsNormal ) return value;

            Fraction f = Remainder( value, Get2Pi( ctx ), ctx );
            if( f.IsZero ) return f;
            var pi = GetPi( ctx );

            return new Fraction( f.N * pi.D * 180, f.D * pi.N, f.E - pi.E, isApprox: true, isSimplified: false ).Reduce( ctx );
        }

        #endregion


        #region Hyperbolics

        enum HypModeEnum
        {
            Series = 1,
            DFloat = 2,
        }

        static HypModeEnum GetHypMode( CalculationContext ctx )
        {
            return HypModeEnum.DFloat;
        }

        /// <summary>
        /// Hyperbolic sine.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <exception cref="ApplicationException"></exception>
        public static Fraction Sinh( Fraction value, CalculationContext ctx )
        {
            HypModeEnum mode = GetHypMode( ctx );

            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return NegativeInfinity;
            if( value.IsPositiveInfinity ) return PositiveInfinity;

            Debug.Assert( value.IsNormal );

            if( value.IsZero ) return value;

            switch( mode )
            {
            case HypModeEnum.Series:
            {
                // https://en.wikipedia.org/wiki/Hyperbolic_functions, "Taylor series expressions"

                Fraction x = value.Simplify( ctx );
                Fraction current = x;
                Fraction sum = current;
                Fraction eps = new( BigInteger.One, ctx.MaxVal, BigInteger.MinusOne );

                for( int i = 3; ; i += 2 )
                {
                    Fraction next = new Fraction( current.N * x.N * x.N, current.D * x.D * x.D * ( i - 1 ) * i, current.E + x.E ).Reduce( ctx );

                    if( Abs( next, ctx ).CompareTo( ctx.Cnc, eps ) < 0 ) break;

                    sum = Add( sum, next, ctx );
                    current = next;
                }

                return sum.UnionApprox( value );
            }
            case HypModeEnum.DFloat:
            {
                // Seems much faster

                int mb = ctx.MaxBytes * 2;

                DFloat f = value.ToDFloat( mb );
                DFloat r = DFloat.Sinh( f, mb );

                return FromDFloat( r ).Reduce( ctx );
            }
            default:
                throw new ApplicationException( "Invalid mode" );
            }
        }

        /// <summary>
        /// Hyperbolic cosine.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        /// <exception cref="ApplicationException"></exception>
        public static Fraction Cosh( Fraction value, CalculationContext ctx )
        {
            HypModeEnum mode = GetHypMode( ctx );

            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return PositiveInfinity;
            if( value.IsPositiveInfinity ) return PositiveInfinity;

            Debug.Assert( value.IsNormal );

            if( value.IsZero ) return One.UnionApprox( value );

            switch( mode )
            {
            case HypModeEnum.Series:
            {
                // https://en.wikipedia.org/wiki/Hyperbolic_functions, "Taylor series expressions"

                Fraction x = value.Simplify( ctx );
                Fraction current = One;
                Fraction sum = current;
                Fraction eps = new( BigInteger.One, ctx.MaxVal, -1 );

                for( int i = 2; ; i += 2 )
                {
                    Fraction next = new Fraction( current.N * x.N * x.N, current.D * x.D * x.D * ( i - 1 ) * i, current.E + x.E ).Reduce( ctx );

                    if( next.CompareTo( ctx.Cnc, eps ) < 0 ) break;

                    sum = Add( sum, next, ctx );
                    current = next;
                }

                return sum.UnionApprox( value );
            }
            case HypModeEnum.DFloat:
            {
                // Seems much faster

                int mb = ctx.MaxBytes * 2;

                DFloat f = value.ToDFloat( mb );
                DFloat r = DFloat.Cosh( f, mb );

                return FromDFloat( r ).Reduce( ctx );
            }
            default:
                throw new ApplicationException( "Invalid mode" );
            }

        }

        /// <summary>
        /// Hyperbolic tangent.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Tanh( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return MinusOne;
            if( value.IsPositiveInfinity ) return One;

            Debug.Assert( value.IsNormal );

            if( value.IsZero ) return value;

            // Seems much faster

            int mb = ctx.MaxBytes * 2;

            DFloat f = value.ToDFloat( mb );
            DFloat r = DFloat.Tanh( f, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        /// <summary>
        /// Hyperbolic cotangent.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction Coth( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return MinusOne;
            if( value.IsPositiveInfinity ) return One;

            Debug.Assert( value.IsNormal );

            if( value.IsZero ) return Undefined; // (or complex infinity)

            // Seems much faster

            int mb = ctx.MaxBytes * 2;

            DFloat f = value.ToDFloat( mb );
            DFloat r = DFloat.Coth( f, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        /// <summary>
        /// Arsinh.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction ArSinh( Fraction value, CalculationContext ctx )
        {
            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return NegativeInfinity;
            if( value.IsPositiveInfinity ) return PositiveInfinity;

            Debug.Assert( value.IsNormal );

            if( value.IsZero ) return value;

            int mb = ctx.MaxBytes * 2;

            DFloat f = value.ToDFloat( mb );
            DFloat r = DFloat.ArSinh( f, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        /// <summary>
        /// Arcosh.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction ArCosh( Fraction value, CalculationContext ctx )
        {
            // https://en.wikipedia.org/wiki/Inverse_hyperbolic_functions, "Definitions in terms of logarithms"

            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Undefined;
            if( value.IsPositiveInfinity ) return PositiveInfinity;

            Debug.Assert( value.IsNormal );

            switch( value.CompareTo( ctx.Cnc, One ) )
            {
            case < 0: // value < 1
                return Undefined;
            case 0: // value == 1
                return Zero.UnionApprox( value );
            }

            int mb = ctx.MaxBytes * 2;

            DFloat f = value.ToDFloat( mb );
            DFloat r = DFloat.ArCosh( f, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        /// <summary>
        /// Artanh.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction ArTanh( Fraction value, CalculationContext ctx )
        {
            // https://en.wikipedia.org/wiki/Inverse_hyperbolic_functions, "Definitions in terms of logarithms"

            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Undefined;
            if( value.IsPositiveInfinity ) return Undefined;

            Debug.Assert( value.IsNormal );

            switch( value.CompareTo( ctx.Cnc, MinusOne ) )
            {
            case < 0: // value < -1
                return Undefined;
            case 0: // value == -1
                return NegativeInfinity;
            }

            switch( value.CompareTo( ctx.Cnc, One ) )
            {
            case > 0: // value > 1
                return Undefined;
            case 0: // value == 1
                return PositiveInfinity;
            }

            int mb = ctx.MaxBytes * 2;

            DFloat f = value.ToDFloat( mb );
            DFloat r = DFloat.ArTanh( f, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        /// <summary>
        /// Arcoth.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="ctx"></param>
        /// <returns></returns>
        public static Fraction ArCoth( Fraction value, CalculationContext ctx )
        {
            // https://en.wikipedia.org/wiki/Inverse_hyperbolic_functions, "Definitions in terms of logarithms"

            if( value.IsUndefined ) return Undefined;
            if( value.IsNegativeInfinity ) return Undefined;
            if( value.IsPositiveInfinity ) return Undefined;

            Debug.Assert( value.IsNormal );

            switch( value.CompareTo( ctx.Cnc, MinusOne ) )
            {
            case 0: // value == -1
                return NegativeInfinity;
            case > 0: // value > -1
                switch( value.CompareTo( ctx.Cnc, One ) )
                {
                case < 0: // value < 1
                    return Undefined;
                case 0: // value == 1
                    return PositiveInfinity;
                }
                break;
            }

            int mb = ctx.MaxBytes * 2;

            DFloat f = value.ToDFloat( mb );
            DFloat r = DFloat.ArCoth( f, mb );

            return FromDFloat( r ).Reduce( ctx );
        }

        #endregion

        #region Gcd, Lcm

        public static Fraction Gcd( CalculationContext ctx, params Fraction[] arguments )
        {
            if( arguments.Length < 2 ) throw new ApplicationException( $"The “gcd” function expects two or more arguments." );

            List<DFloat> numbers = new( );

            foreach( Fraction arg in arguments )
            {
                if( !arg.IsNormal ) return Undefined;

                if( !arg.TryGetInteger( ctx, out DFloat integer ) ) throw new ApplicationException( "The arguments of “gcd” must be appropriate integer numbers." );

                numbers.Add( integer );
            }

            // https://brilliant.org/wiki/greatest-common-divisor/
            // gcd(a,b,c)=gcd(gcd(a,b),c).

            int mb = ctx.MaxBytes * 2;

            DFloat gcd = DFloatUtilities.GreatestCommonDivisor( ctx.Cnc, DFloat.Abs( numbers[0] ), DFloat.Abs( numbers[1] ), mb );

            for( int i = 2; i < numbers.Count; i++ )
            {
                gcd = DFloatUtilities.GreatestCommonDivisor( ctx.Cnc, gcd, DFloat.Abs( numbers[i] ), mb );
            }

            Fraction r = FromDFloat( gcd ); // no 'Reduce'?

            return r;
        }

        public static Fraction Lcm( CalculationContext ctx, params Fraction[] arguments )
        {
            if( arguments.Length < 2 ) throw new ApplicationException( $"The “lcm” function expects two or more arguments." );

            List<DFloat> numbers = new( );

            foreach( Fraction arg in arguments )
            {
                if( !arg.IsNormal ) return Undefined;

                if( !arg.TryGetInteger( ctx, out DFloat integer ) ) throw new ApplicationException( "The arguments of “lcm” must be appropriate integer numbers." );

                numbers.Add( integer );
            }

            // https://brilliant.org/wiki/lowest-common-multiple/
            // lcm(a,b,c)=lcm(lcm(a,b),c).

            int mb = ctx.MaxBytes * 2;

            DFloat lcm = DFloatUtilities.LeastCommonMultiple( ctx.Cnc, DFloat.Abs( numbers[0] ), DFloat.Abs( numbers[1] ), mb );

            for( int i = 2; i < numbers.Count; i++ )
            {
                lcm = DFloatUtilities.LeastCommonMultiple( ctx.Cnc, lcm, DFloat.Abs( numbers[i] ), mb );
            }

            Fraction r = FromDFloat( lcm ); // no 'Reduce'?

            return r;
        }


        #endregion

        #region Conversions

        public static Fraction? TryParse( string s )
        {
            return FractionFormatter.TryParse( s );
        }

        public static Fraction FromDFloat( DFloat f )
        {
            return new Fraction( f.M, BigInteger.One, f.E, isApprox: f.IsApprox, isSimplified: false );
        }

        public DFloat ToDFloat( int maxBytes )
        {
            return DFloat.Div( new DFloat( N, E, IsApprox ), new DFloat( D ), maxBytes );
        }

        public static Fraction FromDouble( double x, CalculationContext ctx )
        {
            // https://stackoverflow.com/questions/389993/extracting-mantissa-and-exponent-from-double-in-c-sharp

            long bits = BitConverter.DoubleToInt64Bits( x );
            //bool negative = ( bits & ( 1L << 63 ) ) != 0;
            int exponent = (int)( bits >> 52 & 0x7ffL );
            long mantissa = bits & 0xfffffffffffffL;

            // Subnormal numbers; exponent is effectively one higher,
            // but there's no extra normalisation bit in the mantissa
            if( exponent == 0 )
            {
                exponent++;
            }
            // Normal numbers; leave exponent as it is but add extra
            // bit to the front of the mantissa
            else
            {
                mantissa = mantissa | 1L << 52;
            }

            // Bias the exponent. It's actually biased by 1023, but we're
            // treating the mantissa as m.0 rather than 0.m, so we need
            // to subtract another 52 from it.
            exponent -= 1075;

            /* Normalize */
            if( mantissa != 0 )
            {
                while( ( mantissa & 1 ) == 0 )
                {    /*  i.e., Mantissa is even */
                    mantissa >>= 1;
                    exponent++;
                }
            }

            return exponent < 0 ?
                    new Fraction( x < 0 ? -mantissa : mantissa, BigInteger.Pow( Bi2, Math.Abs( exponent ) ), BigInteger.Zero ).Simplify( ctx ) :
                    new Fraction( ( x < 0 ? -mantissa : mantissa ) * BigInteger.Pow( Bi2, Math.Abs( exponent ) ), BigInteger.One, BigInteger.Zero ).Simplify( ctx );
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

        public bool TryGetInteger( CalculationContext ctx, out DFloat integer )
        {
            if( !IsNormal )
            {
                integer = DFloat.Zero;

                return false;
            }

            var s = Simplify( ctx );

            if( s.E >= 0 && s.D == 1 )
            {
                integer = new DFloat( s.N, s.E );

                return true;
            }
            else
            {
                if( s.E >= 0 )
                {
                    BigInteger n = s.N;
                    BigInteger d = s.D;
                    BigInteger e = s.E;
                    int mb = ctx.MaxBytes * 2;

                    do
                    {
                        var (q, r) = BigInteger.DivRem( n, d );

                        if( r.IsZero )
                        {
                            integer = new DFloat( q, e );

                            return true;
                        }

                        n *= Bi10;
                        --e;
                    }
                    while( e >= 0 && n.GetByteCount( ) <= mb );
                }

                integer = DFloat.Zero;

                return false;
            }
        }

        /// <summary>
        /// Extract nominator, denominator and exponent.
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


        #region Operators

        [ThreadStatic]
        static CalculationContext? StaticContext;

        public class LocalContext : IDisposable
        {
            CalculationContext? PreviousContext;

            public LocalContext( CalculationContext ctx )
            {
                PreviousContext = StaticContext;
                StaticContext = ctx;
            }

            public void Dispose( )
            {
                StaticContext = PreviousContext;
            }
        }

        public static Fraction operator +( Fraction value )
        {
            return value;
        }

        public static Fraction operator -( Fraction value )
        {
            return Neg( value, StaticContext! );
        }

        public static Fraction operator +( Fraction left, Fraction right )
        {
            return Add( left, right, StaticContext! );
        }

        public static Fraction operator +( Fraction left, BigInteger right )
        {
            return Add( left, new Fraction( right ), StaticContext! );
        }

        public static Fraction operator -( Fraction left, Fraction right )
        {
            return Sub( left, right, StaticContext! );
        }

        public static Fraction operator -( Fraction left, BigInteger right )
        {
            return Sub( left, new Fraction( right ), StaticContext! );
        }

        public static Fraction operator *( Fraction left, Fraction right )
        {
            return Mul( left, right, StaticContext! );
        }

        public static Fraction operator *( Fraction left, BigInteger right )
        {
            return Mul( left, new Fraction( right ), StaticContext! );
        }

        public static Fraction operator /( Fraction left, Fraction right )
        {
            return Div( left, right, StaticContext! );
        }

        public static Fraction operator /( Fraction left, BigInteger right )
        {
            return Div( left, new Fraction( right ), StaticContext! );
        }

        public static Fraction operator %( Fraction left, Fraction right )
        {
            return Remainder( left, right, StaticContext! );
        }

        public static Fraction operator %( Fraction left, BigInteger right )
        {
            return Remainder( left, new Fraction( right ), StaticContext! );
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
