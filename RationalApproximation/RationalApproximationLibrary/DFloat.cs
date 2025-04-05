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

        // from WolframAlpha, "exp(1)":
        static readonly string EString = "2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901157383418793070215408914993488416750924476146066808226480016847741185374234544243710753907774499206955170";
        static readonly Lazy<DFloat> ELarge = new( ( ) => TryParse( EString.Trim( ), int.MaxValue )! );
        static readonly ConcurrentDictionary<int /* maxBytes */, DFloat> ECached = new( );
        public static DFloat GetE( int maxBytes )
        {
            return ECached.GetOrAdd( maxBytes, mb => ELarge.Value.Reduce( mb ) );
        }

        // from WolframAlpha:
        static readonly string Lb10Text = "3.3219280948873623478703194294893901758648313930245806120547563958159347766086252158501397433593701550996573717102502518268240969842635268882753027729986553938519513526575055686430176091900248916669414333740119031241873751097158664675401791896558067358307797";
        static readonly Lazy<(BigInteger n, BigInteger d)> Lb10Fraction = new( ( ) =>
        {
            int dt = Lb10Text.IndexOf( '.' );
            string s = Lb10Text.Remove( dt, 1 );

            return (BigInteger.Parse( s ), BigInteger.Pow( Bi10, s.Length - dt ));
        } );

        // from WolframAlpha:
        const string Ln10String = "2.302585092994045684017991454684364207601101488628772976033327900967572609677352480235997205089598298341967784042286248633409525465082806756666287369098781689482907208325554680843799894826233198528393505308965377732628846163366222287698219886746543667474404243274365155048934314939391479619404400222105101714174800368808";
        static readonly Lazy<DFloat> Ln10Large = new( ( ) => TryParse( Ln10String, int.MaxValue )! );
        static readonly ConcurrentDictionary<int /* maxBytes */, DFloat> Ln10Cached = new( );
        public static DFloat GetLn10( int maxBytes )
        {
            return Ln10Cached.GetOrAdd( maxBytes, mb => Ln10Large.Value.Reduce( mb ) );
        }

        // from WolframAlpha:
        const string Ln2String = "0.693147180559945309417232121458176568075500134360255254120680009493393621969694715605863326996418687542001481020570685733685520235758130557032670751635075961930727570828371435190307038623891673471123350115364497955239120475172681574932065155524734139525882950453007095326366642654104239157814952043740430385500801944";
        static readonly Lazy<DFloat> Ln2Large = new( ( ) => TryParse( Ln2String, int.MaxValue )! );
        static readonly ConcurrentDictionary<int /* maxBytes */, DFloat> Ln2Cached = new( );
        public static DFloat GetLn2( int maxBytes )
        {
            return Ln2Cached.GetOrAdd( maxBytes, mb => Ln2Large.Value.Reduce( mb ) );
        }

        // from WolframAlpha:
        const string PiString = "3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587";
        static readonly Lazy<DFloat> PiLarge = new( ( ) => TryParse( PiString, int.MaxValue )! );
        static readonly ConcurrentDictionary<int /* maxBytes */, DFloat> PiCached = new( );
        static readonly ConcurrentDictionary<int /* maxBytes */, DFloat> TwoPiCached = new( );
        static readonly ConcurrentDictionary<int /* maxBytes */, DFloat> PiOverTwoCached = new( );
        static readonly ConcurrentDictionary<int /* maxBytes */, DFloat> PiOverFourCached = new( );
        public static DFloat GetPi( int maxBytes )
        {
            return PiCached.GetOrAdd( maxBytes, mb => PiLarge.Value.Reduce( mb ) );
        }
        public static DFloat Get2Pi( int maxBytes )
        {
            return TwoPiCached.GetOrAdd( maxBytes, mb => new DFloat( PiLarge.Value.M * 2, PiLarge.Value.E ).Reduce( mb ) );
        }
        public static DFloat GetPiOverTwo( int maxBytes )
        {
            return PiOverTwoCached.GetOrAdd( maxBytes, mb => Div( PiLarge.Value, Two, mb ) );
        }
        public static DFloat GetPiOverFour( int maxBytes )
        {
            return PiOverFourCached.GetOrAdd( maxBytes, mb => Div( PiLarge.Value, Four, mb ) );
        }

        // from WolframAlpha, sqrt(2/pi):
        const string Sqrt2DivPiString = "0.7978845608028653558798921198687637369517172623298693153318516593413158517986036770025046678146138728606051177252703653710219839091116744859924254612510154126905411654409986351290326916150611945072854641673391869565434059983728381269120656178667772134093073055934337386839095423544241306507507382669575011208481114097685";
        static readonly Lazy<DFloat> Sqrt2DivPiLarge = new( ( ) => TryParse( Sqrt2DivPiString, int.MaxValue )! );
        static readonly ConcurrentDictionary<int /* maxBytes */, DFloat> Sqrt2DivPiCached = new( );
        public static DFloat GetSqrt2DivPi( int maxBytes )
        {
            return Sqrt2DivPiCached.GetOrAdd( maxBytes, mb => Sqrt2DivPiLarge.Value.Reduce( mb ) );
        }

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
                BigInteger be =  bits * lb10.d / lb10.n ;
                int e = (int)be;
                BigInteger p = BigInteger.Pow( Bi10, e );

                return (e, p, p / 2);
            }
            };
        }

        static DFloat EvalEps( int maxBytes )
        {
            BigInteger t1 = BigInteger.Pow( Bi2, maxBytes * 8 );
            double t2 = BigInteger.Log10( t1 );

            return new DFloat( BigInteger.One, -(int)Math.Ceiling( t2 ) - 1 );
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

        /// <summary>
        /// Keep the specified number of significant digits.
        /// </summary>
        /// <param name="digitsToKeep">Number of significant decimal digits for M.</param>
        /// <returns></returns>
        public DFloat Keep( int digitsToKeep )
        {
            ArgumentOutOfRangeException.ThrowIfLessThan( digitsToKeep, 1 );

            BigInteger mx = BigInteger.Pow( Bi10, digitsToKeep );
            BigInteger m = BigInteger.Abs( M );

            if( m < mx ) return this;

            BigInteger e = E;
            BigInteger r = BigInteger.Zero;

            do
            {
                (m, r) = BigInteger.DivRem( m, Bi10 );
                ++e;
            } while( m >= mx );

            if( r >= Bi5 )
            {
                ++m; // round

                if( m >= mx ) // when 999 becomes 1000
                {
                    m = BigInteger.Divide( m, Bi10 );
                    ++e;
                }
            }

            return new DFloat( IsNegative ? -m : m, e, IsApprox );
        }

        #region Arithmetics

        /// <summary>
        /// Absolute value.
        /// </summary>
        /// <param name="f"></param>
        /// <returns></returns>
        public static DFloat Abs( DFloat f )
        {
            if( f.M >= 0 )
            {
                return f;
            }
            else
            {
                return new DFloat( -f.M, f.E, f.IsApprox );
            }
        }

        /// <summary>
        /// Negation.
        /// </summary>
        /// <param name="f"></param>
        /// <returns></returns>
        public static DFloat Neg( DFloat f )
        {
            if( f.IsZero )
            {
                return f;
            }
            else
            {
                return new DFloat( -f.M, f.E, f.IsApprox );
            }
        }

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

                return new DFloat(  f1.M < 0  !=  f2.M < 0  ? -m : m, e, f1.IsApprox || f2.IsApprox || is_approx ).Reduce( maxBytes );
            }
        }

        /// <summary>
        /// Inversion (1/x).
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat Inv( DFloat f, int maxBytes )
        {
            return Div( One, f, maxBytes );
        }

        /// <summary>
        /// Addition.
        /// </summary>
        /// <param name="f1"></param>
        /// <param name="f2"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="OverflowException"></exception>
        public static DFloat Add( DFloat f1, DFloat f2, int maxBytes )
        {
            if( f2.IsZero ) return f1;
            if( f1.IsZero ) return f2;

            BigInteger m1, e1;
            BigInteger m2, e2;

            if( f1.E >= f2.E )
            {
                m1 = f1.M;
                e1 = f1.E;
                m2 = f2.M;
                e2 = f2.E;
            }
            else
            {
                m1 = f2.M;
                e1 = f2.E;
                m2 = f1.M;
                e2 = f1.E;
            }

            while( e1 > e2 && m1.GetByteCount( ) < maxBytes + 1 )
            {
                m1 *= Bi10;
                --e1;
            }

            Debug.Assert( e1 >= e2 );

            bool is_approx = false;

            if( e1 > e2 )
            {
                BigInteger diff_e = e1 - e2;

                var ev = EvalPow10( m2.GetByteCount( ) );

                if( diff_e >= ev.e + 2 ) // value too small compared with another one
                {
                    m2 = BigInteger.Zero;
                    is_approx = true;
                }
                else
                {
                    BigInteger d = BigInteger.Pow( Bi10, (int)diff_e );

                    (m2, BigInteger r) = BigInteger.DivRem( m2, d );

                    is_approx = !r.IsZero;

                    if( r >= d / 2 ) // for positive 'm2'
                    {
                        ++m2;
                    }
                    else if( r <= -d / 2 ) // for negative 'm2'
                    {
                        --m2;
                    }
                }

                // (e2 not more relevant)
            }

            BigInteger sum_m = m1 + m2;

            if( e1.GetByteCount( ) > MaxExponentBytes ) throw new OverflowException( "Resulting exponent too large." );

            return new DFloat( sum_m, e1, f1.IsApprox || f2.IsApprox || is_approx ).Reduce( maxBytes );
        }

        /// <summary>
        /// Subtraction.
        /// </summary>
        /// <param name="f1"></param>
        /// <param name="f2"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat Sub( DFloat f1, DFloat f2, int maxBytes )
        {
            return Add( f1, Neg( f2 ), maxBytes );
        }

        /// <summary>
        /// Remainder.
        /// </summary>
        /// <param name="n"></param>
        /// <param name="d"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat Remainder( DFloat n, DFloat d, int maxBytes )
        {
            if( d.IsZero ) throw new InvalidOperationException( "Remainder with denominator zero" );

            DFloat n_abs = Abs( n );
            DFloat d_abs = Abs( d );

            switch( n_abs.CompareTo( d_abs ) )
            {
            case < 0: // ex 12 % 17, remainder is 12
                return n.UnionApprox( d );
            case 0:   // ex 12 % 12, remainder is 0
                return Zero.UnionApprox( n, d );
            }

            // n > d

            n_abs = n_abs.Trim( );
            d_abs = d_abs.Trim( );

            (BigInteger n, BigInteger e) a = (n_abs.M, n_abs.E);
            (BigInteger n, BigInteger e) b = (d_abs.M, d_abs.E);

            BigInteger e = -BigInteger.Min( a.e, b.e );

            a.e += e;
            b.e += e;

            Debug.Assert( a.e >= 0 );
            Debug.Assert( b.e >= 0 ); //

            var a2 = a;
            BigInteger b2 = b.n * BigInteger.Pow( Bi10, (int)b.e );
            BigInteger r = BigInteger.Zero;

            const int se = 300;
            BigInteger s = BigInteger.Pow( Bi10, se );

            for(; ; )
            {
                r = BigInteger.Remainder( a2.n, b2 );

                if( a2.e == 0 ) break;

                BigInteger k = BigInteger.Min( a2.e, se );

                a2.e -= k;
                a2.n = r * ( k == se ? s : BigInteger.Pow( Bi10, (int)k ) );
            }

            return new DFloat( n.IsNegative ? -r : r, -e, n.IsApprox || d.IsApprox ).Reduce( maxBytes );
        }

        #endregion

        #region Rounding

        /// <summary>
        /// Truncate (toward zero).
        /// </summary>
        /// <param name="f"></param>
        /// <returns></returns>
        public static DFloat Truncate( DFloat f )
        {
            if( f.E >= 0 ) return f; // +/- 123, 123e100

            DFloat a = Abs( f.Trim( ) );

            if( a.CompareTo( One ) < 0 ) return Zero.UnionApprox( f ); // +/- 123e-3, 123e-100, 0

            // +/- 123e-1

            Debug.Assert( f.E < 0 );

            BigInteger p = BigInteger.Pow( Bi10, (int)-f.E );
            BigInteger q = BigInteger.Divide( f.M, p );

            Debug.Assert( !q.IsZero );

            return new DFloat( q, BigInteger.Zero, f.IsApprox );
        }

        #endregion

        #region Exponentiation

        /// <summary>
        /// Exponentiation to integer power.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="p"></param>
        /// <param name="ispApprox"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        internal static DFloat IntPow( DFloat f, BigInteger p, bool ispApprox, int maxBytes )
        {
            if( f.IsZero )
            {
                if( p.IsZero ) throw new InvalidOperationException( "0 raised to 0" );
                if( p < 0 ) throw new InvalidOperationException( $"0 raised to negative ({p})" );

                return Zero.UnionApprox( f ); // 0**123
            }

            if( p.IsZero ) return One.UnionApprox( ispApprox ); // 123**0
            if( p.IsOne ) return f.UnionApprox( ispApprox ); // 123**1
            if( f.QuickTestOne ) return f; // 1**123, 1**-123

            if( f.QuickTestMinusOne )
            {
                if( p.IsEven )
                {
                    return One.UnionApprox( f ); // -1**124
                }
                else
                {
                    return MinusOne.UnionApprox( f ); // -1**123
                }
            }

            if( p < 0 )
            {
                DFloat r = IntPowPositive( f, -p, maxBytes + 2 );
                r = Inv( r, maxBytes );

                return r.UnionApprox( f.IsApprox || ispApprox );
            }
            else
            {
                DFloat r = IntPowPositive( f, p, maxBytes );

                return r.UnionApprox( f.IsApprox || ispApprox );
            }

            static DFloat IntPowPositive( DFloat f, BigInteger p, int maxBytes )
            {
                long bit_length = p.GetBitLength( );
                byte[] bytes = p.ToByteArray( );

                // TODO: reconsider the parallel version, which seemed slower

                DFloat a = Abs( f );
                DFloat result = One;

                for( long i = 0; i < bit_length; ++i )
                {
                    if( ( bytes[i / 8] & (byte)( 1 << (int)( i % 8 ) ) ) != 0 )
                    {
                        result = Mul( result, a, maxBytes + 2 );
                    }

                    a = Mul( a, a, maxBytes + 2 );
                }

                if( f.IsNegative && !p.IsEven ) result = Neg( result );

                return result.Reduce( maxBytes );
            }
        }

        /// <summary>
        /// Exponentiation.
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        /// <exception cref="OverflowException"></exception>
        public static DFloat Pow( DFloat x, DFloat y, int maxBytes )
        {
            if( x.IsZero )
            {
                if( y.IsNegative ) throw new InvalidOperationException( "zero raised to negative" );
                if( y.IsZero ) throw new InvalidOperationException( "zero raised to zero" ); // (or 1 in some theories)

                Debug.Assert( y.IsPositiveNonZero );

                return Zero.UnionApprox( x );
            }

            if( y.IsZero ) // x ** 0
            {
                return One.UnionApprox( y );
            }

            if( x.IsOne( ) )  // 1 ** y
            {
                return x;
            }

            //...........
            // TODO: check for 2, e, etc.

            y = y.Trim( );

            if( y.E >= 0 ) // to integer power
            {
                if( x.IsMinusOne( ) ) // -1 to integer power
                {
                    if( y.E > 0 || y.M.IsEven )
                    {
                        // -1 to integer even power

                        return One.UnionApprox( x, y );
                    }
                    else
                    {
                        // -1 to integer odd power

                        return MinusOne.UnionApprox( x, y );
                    }
                }
                else // (!= -1) to integer power
                {
                    bool y_is_negative;
                    DFloat abs_y;

                    if( y.IsNegative )
                    {
                        y_is_negative = true;
                        abs_y = Neg( y );
                    }
                    else
                    {
                        y_is_negative = false;
                        abs_y = y;
                    }

                    DFloat p1 = IntPow( x, abs_y.M, false, maxBytes + 2 );
                    if( abs_y.E > int.MaxValue ) throw new OverflowException( $"Too large exponent: {abs_y.E}" );
                    BigInteger p2 = BigInteger.Pow( Bi10, (int)abs_y.E );

                    if( y_is_negative )
                    {
                        DFloat r = IntPow( p1, p2, false, maxBytes + 2 );
                        r = Inv( r, maxBytes );

                        return r.UnionApprox( x, y );
                    }
                    else
                    {
                        DFloat r = IntPow( p1, p2, false, maxBytes );

                        return r.UnionApprox( x, y );
                    }
                }
            }
            else
            {
                // x**y = e**(y*ln(x))

                DFloat ln_x = Ln( x, maxBytes + 2 );
                DFloat m = Mul( y, ln_x, maxBytes + 2 );
                DFloat r = Exp( m, maxBytes );

                return r;
            }
        }

        /// <summary>
        /// Natural exponentiation (power of e).
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="OverflowException"></exception>
        public static DFloat Exp( DFloat f, int maxBytes )
        {
            if( f.IsZero ) return One.UnionApprox( f );
            if( f.IsOne( ) ) return ELarge.Value.Reduce( maxBytes ).AsApprox( );
            if( f.IsMinusOne( ) ) return Inv( ELarge.Value, maxBytes ).AsApprox( );

            DFloat t = f.Trim( );
            int mb = maxBytes + 4;

            if( f.IsNegative )
            {
                t = Neg( t );
                DFloat r = ExpPositive( t, mb );
                r = Inv( r, maxBytes );

                return r;
            }
            else
            {
                DFloat r = ExpPositive( t, mb ).Reduce( maxBytes );

                return r;
            }

            static DFloat ExpPositive( DFloat f, int maxBytes )
            {
                if( f.E >= 0 ) // e to positive integer power
                {
                    DFloat number_e = GetE( maxBytes + 2 );
                    DFloat r = IntPow( number_e, f.M, false, maxBytes + 2 );

                    (BigInteger q, BigInteger r) ee = BigInteger.DivRem( f.E, BucketSize );

                    if( ee.q > long.MaxValue )
                    {
                        throw new OverflowException( $"Too large exponent: {f.E}" );
                    }

                    for( long i = 0; i < (long)ee.q; ++i )
                    {
                        r = IntPow( r, TenPowBucket, false, maxBytes + 2 );
                    }

                    if( !ee.r.IsZero )
                    {
                        r = IntPow( r, BigInteger.Pow( Bi10, checked((int)ee.r) ), false, maxBytes );
                    }
                    else
                    {
                        r = r.Reduce( maxBytes );
                    }

                    return r.AsApprox( );
                }
                else
                {
                    // e to non-integer 

                    DFloat truncated = Truncate( f );
                    DFloat diff = Sub( f, truncated, maxBytes + 2 );

                    Debug.Assert( diff.IsPositiveOrZero );

                    // e to integer (truncated f)

                    DFloat r = IntPow( GetE( maxBytes + 2 ), truncated.M, false, maxBytes + 2 );

                    // to 10**mE

                    (BigInteger q, BigInteger r) ee = BigInteger.DivRem( truncated.E, BucketSize );

                    if( ee.q > long.MaxValue )
                    {
                        throw new OverflowException( $"Too large exponent: {truncated.E}" );
                    }

                    for( long i = 0; i < (long)ee.q; ++i )
                    {
                        r = IntPow( r, TenPowBucket, false, maxBytes + 2 );
                    }

                    if( !ee.r.IsZero ) r = IntPow( r, BigInteger.Pow( Bi10, checked((int)ee.r) ), false, maxBytes + 2 );

                    if( !diff.IsZero )
                    {
                        // WA: e^x = sum_(k=0)^∞ x^k/(k!)

                        DFloat current = diff;
                        DFloat sum = current;

                        var ev = EvalPow10( maxBytes + 1 );
                        DFloat eps = Inv( new DFloat( ev.p ), maxBytes + 1 );

                        for( int i = 2; ; ++i )
                        {
                            var t1 = Mul( current, diff, maxBytes + 2 );
                            var next = Div( t1, new DFloat( i ), maxBytes + 2 );

                            if( next.CompareTo( eps ) <= 0 ) break; //

                            sum = Add( sum, next, maxBytes + 2 );
                            current = next;
                        }

                        sum = Add( sum, One, maxBytes + 2 );
                        r = Mul( r, sum, maxBytes + 2 );
                    }

                    return r.Reduce( maxBytes ).AsApprox( );
                }
            }
        }

        /// <summary>
        /// Square root using Heron's method.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat SqrtHeron( DFloat f, int maxBytes )
        {
            if( f.IsNegative ) throw new InvalidOperationException( $"Sqrt of negative ({f})" );
            if( f.IsZero ) return Zero.UnionApprox( f );
            if( f.IsOne( ) ) return f;

            // https://en.wikipedia.org/wiki/Methods_of_computing_square_roots, "Heron's method"

            DFloat eps = EvalEps( maxBytes );
            int mb = maxBytes + 2;

            f = f.Trim( );

            // make sure the E is even and keep the mantissa only
            DFloat s;
            BigInteger e;
            if( f.E.IsEven )
            {
                s = new DFloat( f.M );
                e = f.E;
            }
            else
            {
                s = new DFloat( f.M * 10 );
                e = f.E - 1;
            }

            DFloat current = One;

            for(; ; )
            {
                DFloat next = Div( Add( current, Div( s, current, mb + 1 ), mb + 1 ), Two, mb );
                DFloat diff = Abs( Sub( next, current, mb ) );

                if( diff.CompareTo( eps ) < 0 ) break;

                current = next;
            }

            DFloat r = new( current.M, current.E + e / 2, isApprox: current.IsApprox );
            r = r.Reduce( maxBytes );

            // check if result is exact, by multiplication
            if( !f.IsApprox && r.IsApprox )
            {
                DFloat rna = r.AsNonApprox( );
                DFloat p = Mul( rna, rna, maxBytes );

                if( !p.IsApprox && p.Equals( f ) ) r = rna;
            }

            return r;
        }

        /// <summary>
        /// Square root using binary search.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat SqrtBinarySearch( DFloat f, int maxBytes )
        {
            if( f.IsNegative ) throw new InvalidOperationException( $"Sqrt of negative" );
            if( f.IsZero ) return Zero.UnionApprox( f );
            if( f.IsOne( ) ) return f;

            int mb = maxBytes + 2;
            DFloat eps = EvalEps( maxBytes );
            DFloat left;
            DFloat right;

            if( f.CompareTo( One ) < 0 )
            {
                left = Zero;
                right = One;
            }
            else
            {
                left = One;
                right = f;
            }

            for(; ; )
            {
                DFloat mid = Div( Add( left, right, mb ), Two, mb );
                DFloat square = new( mid.M * mid.M, mid.E * 2 );

                switch( square.CompareTo( f ) )
                {
                case < 0:
                    left = mid;
                    break;
                case > 0:
                    right = mid;
                    break;
                default: // ==0
                    return mid.Reduce( maxBytes );
                }

                DFloat diff = Sub( f, square, mb );

                if( Abs( diff ).CompareTo( eps ) < 0 ) return mid.Reduce( maxBytes ).AsApprox( );
            }

        }

        #endregion

        #region Logarithm

        /// <summary>
        /// Natural logarithm (base-e).
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat Ln( DFloat f, int maxBytes )
        {
            if( f.IsZero ) throw new InvalidOperationException( "Ln(0)" );
            if( f.IsNegative ) throw new InvalidOperationException( $"Ln of negative ({f})" );
            if( f.IsOne( ) ) return Zero.UnionApprox( f );

            // https://en.wikipedia.org/wiki/Natural_logarithm, "High precision", 

            DFloat x = f.Trim( );

            // ln(a*10**b) = ln(a) + b * ln(10)

            DFloat ln_m = LnInternal( new DFloat( x.M ), maxBytes + 2 );
            DFloat ln_10 = GetLn10( maxBytes + 2 ); // TODO: use all of the bytes?
            DFloat t = Mul( new DFloat( x.E ), ln_10, maxBytes + 2 );
            DFloat r = Add( ln_m, t, maxBytes );

            return r;

            static DFloat LnInternal( DFloat x, int maxBytes )
            {
                // actually 'x' is integer, >= 1

                int mb = maxBytes + 2;
                DFloat y = Zero;
                var ev = EvalPow10( maxBytes + 1 );
                DFloat eps = Inv( new DFloat( ev.p ), mb );

                for(; ; )
                {
                    var exp = Exp( y, mb );
                    var n = Sub( x, exp, mb );
                    n = Mul( n, Two, mb );
                    var d = Add( x, exp, mb );
                    var a = Div( n, d, mb );

                    if( Abs( a ).CompareTo( eps ) <= 0 ) break;

                    y = Add( y, a, mb );
                }

                return y.Reduce( maxBytes ).AsApprox( );
            }
        }

        /// <summary>
        /// Common (Decimal) logarithm (base-10).
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat Lg( DFloat f, int maxBytes )
        {
            if( f.IsZero ) throw new InvalidOperationException( "Lg(0)." );
            if( f.IsNegative ) throw new InvalidOperationException( "Lg of negative." );
            if( f.IsOne( ) ) return Zero.UnionApprox( f );

            // changing the base: log(x, base1) = log(x, base2) / log(base1, base2)

            int mb = maxBytes + 4;
            DFloat ln10 = GetLn10( mb );

            return Div( Ln( f, mb ), ln10, maxBytes );
        }

        /// <summary>
        /// Binary logaritm (base-2).
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat Lb( DFloat f, int maxBytes )
        {
            if( f.IsZero ) throw new InvalidOperationException( "Lb(0)." );
            if( f.IsNegative ) throw new InvalidOperationException( "Lb of negative." );
            if( f.IsOne( ) ) return Zero.UnionApprox( f );

            // changing the base: log(x, base1) = log(x, base2) / log(base1, base2)

            int mb = maxBytes + 4;
            DFloat ln2 = GetLn2( mb );

            return Div( Ln( f, mb ), ln2, maxBytes );
        }

        /// <summary>
        /// Logarithm with any base.
        /// </summary>
        /// <param name="base"></param>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat Log( DFloat @base, DFloat f, int maxBytes )
        {
            if( @base.CompareTo( Zero ) <= 0 ) throw new InvalidOperationException( "Base of Log cannot be zero or negative." );
            if( @base.IsOne( ) ) throw new InvalidOperationException( "Base of Log cannot be 1." );
            if( f.IsZero ) throw new InvalidOperationException( "Log(0)." );
            if( f.IsNegative ) throw new InvalidOperationException( $"Log of negative." );
            if( f.IsOne( ) ) return Zero.UnionApprox( f );

            if( @base.Equals( Ten ) ) return Lg( f, maxBytes ).UnionApprox( @base );
            if( @base.Equals( Two ) ) return Lb( f, maxBytes ).UnionApprox( @base );

            // changing the base: log(x, base1) = log(x, base2) / log(base1, base2)

            int mb = maxBytes + 4;
            DFloat n = Ln( f, mb );
            DFloat d = Ln( @base, mb );

            DFloat result = Div( n, d, maxBytes ).UnionApprox( @base );

            return result;
        }

        #endregion

        #region Factorial and Gamma

        /// <summary>
        /// Factorial of positive integer.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        internal static DFloat FactorialOfPositiveInteger( DFloat f, int maxBytes )
        {
            if( f.IsZero ) return One.UnionApprox( f );
            if( f.IsOne( ) ) return f;

            DFloat n = f.Trim( );
            Debug.Assert( n.E >= 0 ); // is integer
            Debug.Assert( n.IsPositiveNonZero );

            if( n.CompareTo( new DFloat( 1_000_000 ) ) <= 0 )
            {
                return FactorialOfPositiveIntegerBasedOnMul( n, maxBytes );
            }
            else
            {
                // large number; use Nemes's approximation

                // n! = G(n+1)

                DFloat n2 = Add( n, One, maxBytes + 4 );
                DFloat g = GammaNemesPositive( n2, maxBytes );

                return g;
            }
        }

        internal static DFloat FactorialOfPositiveIntegerBasedOnMul( DFloat f, int maxBytes )
        {
            Debug.Assert( f.IsPositiveOrZero );

            DFloat n = f.Trim( );
            Debug.Assert( n.E >= 0 ); // is integer

            BigInteger k = n.M * BigInteger.Pow( Bi10, (int)n.E );

            DFloat r = One;

            for( BigInteger i = k; i > 1; --i )
            {
                r = new DFloat( r.M * i, r.E ).Reduce( maxBytes + 2 );
            }

            r = r.Reduce( maxBytes ).UnionApprox( f );

            return r;
        }

        /// <summary>
        /// Double factorial of positive integer.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        internal static DFloat DoubleFactorialOfPositiveInteger( DFloat f, int maxBytes )
        {
            if( f.IsZero ) return One.UnionApprox( f );
            if( f.IsOne( ) ) return f;

            DFloat n = f.Trim( );
            Debug.Assert( n.E >= 0 ); // is integer
            Debug.Assert( n.IsPositiveNonZero );

            if( n.CompareTo( new DFloat( 10_000_000 ) ) <= 0 )
            {
                return DoubleFactorialOfPositiveIntegerBasedOnMul( n, maxBytes );
            }
            else
            {
                // large number; use approximation
                // https://en.wikipedia.org/wiki/Double_factorial, "Asymptotic"

                // less precise:
                //return DoubleFactorialOfNonInteger( f, maxBytes );

                int mb = maxBytes + 4;

                DFloat n_div_e = Div( n, GetE( mb ), mb );
                DFloat n_div_2 = Div( n, Two, mb );
                DFloat r = Pow( n_div_e, n_div_2, mb );

                if( n.E > 0 || n.M.IsEven )
                {
                    // even

                    DFloat t = Mul( GetPi( mb ), n, mb );
                    t = SqrtHeron( t, mb );
                    r = Mul( t, r, maxBytes );
                }
                else
                {
                    // odd

                    Debug.Assert( n.E == 0 && !n.M.IsEven );

                    DFloat t = Mul( Two, n, mb );
                    t = SqrtHeron( t, mb );
                    r = Mul( t, r, maxBytes );
                }

                r = r.Keep( 7 ); // TODO: how many digits to keep?

                return r.AsApprox( );
            }
        }

        internal static DFloat DoubleFactorialOfPositiveIntegerBasedOnMul( DFloat f, int maxBytes )
        {
            Debug.Assert( f.IsPositiveOrZero );

            DFloat n = f.Trim( );
            Debug.Assert( n.E >= 0 ); // is integer

            BigInteger k = n.M * BigInteger.Pow( Bi10, (int)n.E );

            DFloat r = One;

            for( BigInteger i = k; i > 1; i -= 2 )
            {
                r = new DFloat( r.M * i, r.E ).Reduce( maxBytes + 2 );
            }

            r = r.Reduce( maxBytes ).UnionApprox( f );

            return r;
        }

        internal static DFloat DoubleFactorialOfNonInteger( DFloat z, int maxBytes )
        {
            if( z.M == 0 || z.E >= 0 ) throw new ArgumentException( "Non-integer expected" );

            // https://mathworld.wolfram.com/DoubleFactorial.html

            int mb = maxBytes + 4;

            DFloat piz = Mul( GetPi( int.MaxValue ), z, mb );
            DFloat cos_piz = Cos( piz, mb );

            DFloat p1 = Add( One, Mul( Two, z, mb ), mb );
            p1 = Sub( p1, cos_piz, mb );
            p1 = Div( p1, Four, mb );

            DFloat p2 = Div( Sub( cos_piz, One, mb ), Four, mb );

            p1 = Pow( Two, p1, mb );
            p2 = Pow( GetPi( int.MaxValue ), p2, mb );

            DFloat a = Add( One, Div( z, Two, mb ), mb );
            DFloat g = Gamma( a, mb );

            DFloat r = Mul( Mul( p1, p2, mb ), g, mb ).Reduce( maxBytes );

            return r.AsApprox( );
        }

#if false
        public static DFloat GammaGauss( DFloat f, int maxBytes )
        {
            // TODO: for positive integers, use factorial

            // https://en.wikipedia.org/wiki/Gamma_function, "19th century: Gauss, Weierstrass and Legendre"

            DFloat z = f.Trim( );

            const int m = 10_000_000;

            DFloat n = One;
            DFloat d = z;

            for( int i = 1; i <= m; ++i )
            {
                DFloat fi = new( i );
                n = Mul( n, fi, maxBytes + 3 );

                DFloat a = Add( z, fi, maxBytes + 2 );
                d = Mul( d, a, maxBytes + 3 );
            }

            DFloat p = Pow( new DFloat( m ), z, maxBytes + 3 );
            DFloat r = Mul( p, n, maxBytes );
            r = Div( r, d, maxBytes );

            return r.AsApprox();
        }

        public static DFloat GammaEuler( DFloat f, int maxBytes )
        {
            // TODO: for positive integers, use factorial

            // https://en.wikipedia.org/wiki/Gamma_function, "Euler's definition as an infinite product", "The infinite product for the reciprocal"

            DFloat z = f.Trim( );
            DFloat r = One;

            for( int n = 1; n <= 10_000_000; ++n )
            {
                DFloat fn = new( n );
                //var t1 = Div( Add( fn, z, maxBytes + 3 ), fn, maxBytes + 3 );
                var a = Add( fn, z, maxBytes + 3 );
                var t2 = Div( new DFloat( n + 1 ), fn, maxBytes + 3 );
                var p = Pow( t2, z, maxBytes + 3 );
                var d = Mul( fn, p, maxBytes + 3 );
                d = Div( a, d, maxBytes + 3 );
                r = Mul( r, d, maxBytes + 3 );
            }

            r = Mul( r, z, maxBytes + 3 );
            r = Inv( r, maxBytes );

            return r.AsApprox();
        }
#endif

        public static DFloat GammaContinuedFraction1( DFloat f, int maxBytes, int start_c = 1111 )
        {
            // https://en.wikipedia.org/wiki/Gamma_function, "Continued fraction representation"

            DFloat z = f.Trim( );
            int mb = maxBytes + 5;

            //const int start_c = 555;// 1111; // odd?

            DFloat t = Sub( new DFloat( 2 + 2 * start_c ), z, mb );

            for( int c = start_c; c >= 1; --c )
            {
                DFloat fc = new DFloat( c );
                t =
                    Add(
                        Sub( new DFloat( 2 + 2 * ( c - 1 ) ), z, mb ),
                        Mul( fc, Div( Sub( z, fc, mb ), t, mb ), mb ),
                        mb );
            }

            DFloat g1 = Inv( t, mb );

            DFloat u = Add( z, new DFloat( start_c + 1 ), mb );

            for( int c = start_c - 1; c >= 0; --c )
            {
                DFloat s;
                if(  c % 2  != 0 )
                {
                    s = new DFloat( ( c + 1 ) / 2 );
                }
                else
                {
                    s = Add( z, new DFloat( c / 2 ), mb );
                }

                DFloat d = Div( s, u, mb );

                if(  c % 2  == 0 ) d = Neg( d );

                u =
                    Add(
                        Add( z, new DFloat( c ), mb ),
                        d,
                        mb );
            }

            DFloat g2 = Inv( u, mb );

            DFloat r = Add( g1, g2, mb );
            r = Div( r, GetE( mb ), maxBytes );

            Debug.WriteLine( $"{f.ToDouble( )} | {t.ToDouble( )} | {u.ToDouble( )} | {r.ToDouble( )}" );

            return r.UnionApprox( f );
        }

        public static DFloat GammaContinuedFraction2( DFloat f, int maxBytes, int start_c = 1111 )
        {
            // https://en.wikipedia.org/wiki/Gamma_function, "Continued fraction representation"

            DFloat z = f.Trim( );
            int mb = maxBytes + 5;

            //const int start_c = 1111; // odd?

            DFloat t = Sub( new DFloat( 2 + 2 * start_c ), z, mb );

            for( int c = start_c; c >= 1; --c )
            {
                DFloat fc = new DFloat( c );
                t =
                    Add(
                        Sub( new DFloat( 2 + 2 * ( c - 1 ) ), z, mb ),
                        Div( Mul( fc, Sub( z, fc, mb ), mb ), t, mb ),
                        mb );
            }

            DFloat g1 = Inv( t, mb );

            DFloat u = Add( z, new DFloat( start_c ), mb );

            for( int c = start_c - 1; c >= 0; --c )
            {
                DFloat s;
                if(  c % 2  != 0 )
                {
                    s = new DFloat( ( c + 1 ) / 2 );
                }
                else
                {
                    s = Add( z, new DFloat( c / 2 ), mb );
                }

                DFloat d = Div( s, u, mb );

                if(  c % 2  == 0 ) d = Neg( d );

                u =
                    Add(
                        Add( z, new DFloat( c ), mb ),
                        d,
                        mb );
            }

            DFloat g2 = Inv( u, mb );

            DFloat r = Add( g1, g2, mb );
            r = Div( r, GetE( mb ), maxBytes );

            Debug.WriteLine( $"{f.ToDouble( )} | {t.ToDouble( )} | {u.ToDouble( )} | {r.ToDouble( )}" );

            return r.UnionApprox( f );
        }

        /// <summary>
        /// Gamma function.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        internal static DFloat Gamma( DFloat f, int maxBytes )
        {
            if( f.IsZero ) throw new InvalidOperationException( "Gamma(0)" );
            //if( f.QuickTestOne ) return One;

            DFloat z = f.Trim( );

            if( z.E >= 0 ) // is integer
            {
                Debug.Assert( !z.IsZero );

                if( z.IsPositiveNonZero )
                {
                    // positive integer
                    // G(n) = (n-1)!

                    return FactorialOfPositiveInteger( Add( z, MinusOne, maxBytes + 2 ), maxBytes );
                }
                else
                {
                    // negative integer

                    throw new InvalidOperationException( "Gamma of negative integer" );
                }
            }

            // not integer

            if( !z.IsNegative )
            {
                // positive non-integer

                if( z.CompareTo( new DFloat( 100 ) ) > 0 )
                {
                    // large 

                    // Stirling-like

                    return GammaNemesPositive( z, maxBytes );
                }
                else if( z.CompareTo( Ten ) > 0 ) // ...............
                {
                    // not so large and not so small;
                    // use a smaller value: G(z) = G(z-1)*(z-1) several times

                    DFloat r = One;
                    int mb = maxBytes + 4;
                    DFloat eps = EvalEps( mb );

                    for( int i = 1; ; ++i )
                    {
                        DFloat t = Add( z, new DFloat( -i ), mb ); // z-i
                        r = Mul( r, t, mb ); // ..*(z-i)

                        if( t.CompareTo( Ten ) <= 0 ) //..........
                        {
                            // t is small enough

                            DFloat g = GammaContinuedFractionThompsonBarnettModifiedLentz( t, mb, eps );

                            r = Mul( g, r, maxBytes );

                            break;
                        }
                    }

                    return r.UnionApprox( f );
                }
                else
                {
                    // small 

                    // TODO: if too close to 0

                    DFloat eps = EvalEps( maxBytes );

                    return GammaContinuedFractionThompsonBarnettModifiedLentz( z, maxBytes, eps );
                }
            }
            else
            {
                // negative non-integer

                if( z.CompareTo( new DFloat( -100 ) ) < 0 )
                {
                    // large abs

                    // Stirling-like

                    return GammaNegativeOnNemes( z, maxBytes );
                }
                else if( z.CompareTo( new DFloat( -10 ) ) < 0 ) // ...............
                {
                    // not so large and not so small;
                    // use a smaller abs value: G(z) = G(z+1)/z several times

                    DFloat r = z;
                    int mb = maxBytes + 4;
                    DFloat eps = EvalEps( mb );

                    for( int i = 1; ; ++i )
                    {
                        DFloat t = Add( z, new DFloat( i ), mb ); // z+i

                        if( t.CompareTo( new DFloat( -10 ) ) >= 0 ) //..........
                        {
                            // abs(t) is small enough

                            DFloat g = GammaContinuedFractionThompsonBarnettModifiedLentz( t, mb, eps );

                            r = Div( g, r, maxBytes );

                            return r.UnionApprox( f );
                        }
                        else
                        {
                            r = Mul( r, t, mb );
                        }
                    }
                }
                else
                {
                    // small abs

                    // TODO: if too close to 0

                    DFloat eps = EvalEps( maxBytes );

                    return GammaContinuedFractionThompsonBarnettModifiedLentz( z, maxBytes, eps );
                }
            }
        }

        internal static DFloat GammaContinuedFractionThompsonBarnettModifiedLentz( DFloat f, int maxBytes, DFloat eps )
        {
            // https://en.wikipedia.org/wiki/Gamma_function, "Continued fraction representation"

            if( f.IsZero ) throw new InvalidOperationException( "Gamma(0)" );
            //if( f.QuickTestOne ) return One;

            DFloat z = f.Trim( );

            int mb = maxBytes + 4;
            //DFloat eps = new( 1, -20 ); //..........
            DFloat tiny = Mul( eps, new DFloat( BigInteger.One, -20 ), mb ); // must be less than eps //..........

            DFloat t = ThompsonBarnettModifiedLentz( eps, tiny, mb,
                ( j, mb ) => GetA_Part1( z, j, mb ),
                ( j, mb ) => GetB_Part1( z, j, mb ) );

            DFloat u = ThompsonBarnettModifiedLentz( eps, tiny, mb,
                ( j, mb ) => GetA_Part2( z, j, mb ),
                ( j, mb ) => GetB_Part2( z, j, mb ) );

            DFloat g1 = Inv( t, mb );
            DFloat g2 = Inv( u, mb );

            DFloat r = Add( g1, g2, mb );
            r = Div( r, GetE( mb ), maxBytes );

            //Debug.WriteLine( $"{f.ToDouble( )} | {t.ToDouble( )} | {u.ToDouble( )} | {r.ToDouble( )}" );

            return r.UnionApprox( f );

            static DFloat GetA_Part1( DFloat z, int j, int mb )
            {
                Debug.Assert( j >= 1 );

                DFloat aj = Mul( new DFloat( j ), Sub( z, new DFloat( j ), mb ), mb );

                return aj;
            }

            static DFloat GetB_Part1( DFloat z, int j, int mb )
            {
                Debug.Assert( j >= 0 );

                DFloat bj = Sub( new DFloat( 2 + j * 2 ), z, mb );

                return bj;
            }

            static DFloat GetA_Part2( DFloat z, int j, int mb )
            {
                Debug.Assert( j >= 1 );

                DFloat aj;

                if(  j % 2  != 0 )
                {
                    // j = 1, 3, 5, ...

                    aj = Neg( Add( z, new DFloat( ( j - 1 ) / 2 ), mb ) );
                }
                else
                {
                    // j = 2, 4, 6, ...

                    aj = new DFloat( j / 2 );
                }

                return aj;
            }

            static DFloat GetB_Part2( DFloat z, int j, int mb )
            {
                Debug.Assert( j >= 0 );

                DFloat bj = Add( z, new DFloat( j ), mb );

                return bj;
            }
        }

        static DFloat ThompsonBarnettModifiedLentz( DFloat eps, DFloat tiny, int mb,
            Func<int, int, DFloat> GetA,
            Func<int, int, DFloat> GetB )
        {
            // Continued fraction: f(x) = b0 + a1 / (b1 + a2 / (b2 + a3 / (b3 + a4 / (b4 + a5 / (b5 + ···
            //
            // https://phys.uri.edu/nigh/NumRec/bookfpdf/f5-2.pdf
            //
            // Thompson and Barnett 
            /* Quote:
                In detail, the modified Lentz’s algorithm is this:
                • Set f0 = b0; if b0 = 0 set f0 = tiny.
                • Set C0 = f0.
                • Set D0 = 0.
                • For j = 1, 2,...
                    Set Dj = bj + ajDj−1.
                    If Dj = 0, set Dj = tiny.
                    Set Cj = bj + aj/Cj−1.
                    If Cj = 0 set Cj = tiny.
                    Set Dj = 1/Dj.
                    Set ∆j = CjDj.
                    Set fj = fj−1∆j .
                    If |∆j − 1| < eps then exit.
                Here eps is your floating-point precision, say 10−7 or 10−15. The parameter tiny
                should be less than typical values of eps|bj|, say 10−30.
                The above algorithm assumes that you can terminate
            */

            Debug.Assert( tiny.CompareTo( eps ) < 0 );

            DFloat b = GetB( 0, mb );

            DFloat f = b;
            if( f.IsZero ) f = tiny;
            DFloat C = f;
            DFloat D = Zero;

            for( int j = 1; ; ++j )
            {
                DFloat aj = GetA( j, mb );
                DFloat bj = GetB( j, mb );

                D = Add( bj, Mul( aj, D, mb ), mb );
                if( D.IsZero ) D = tiny;

                C = Add( bj, Div( aj, C, mb ), mb );
                if( C.IsZero ) C = tiny;

                D = Inv( D, mb );

                DFloat delta = Mul( C, D, mb );

                f = Mul( f, delta, mb );

                DFloat diff = Abs( Sub( delta, One, mb ) );
                if( diff.CompareTo( eps ) < 0 ) break;
            }

            return f;
        }

        //// https://oeis.org/A001163, https://oeis.org/A001164
        //static readonly BigInteger[] GammaStirlingNumerators = { /*1,*/ 1, 1, -139, -571, 163879, 5246819, -534703531, -4483131259, 432261921612371, 6232523202521089,
        //        BigInteger.Parse("-25834629665134204969"), BigInteger.Parse("-1579029138854919086429"), BigInteger.Parse("746590869962651602203151"),
        //        BigInteger.Parse("1511513601028097903631961"), BigInteger.Parse("-8849272268392873147705987190261"),
        //        BigInteger.Parse("-142801712490607530608130701097701") };
        //static readonly BigInteger[] GammaStirlingDenominators = { /*1,*/ 12, 288, 51840, 2488320, 209018880, 75246796800, 902961561600, 86684309913600, 514904800886784000,
        //        BigInteger.Parse("86504006548979712000"), BigInteger.Parse("13494625021640835072000"), BigInteger.Parse("9716130015581401251840000"),
        //        BigInteger.Parse("116593560186976815022080000"), BigInteger.Parse("2798245444487443560529920000"), BigInteger.Parse("299692087104605205332754432000000"),
        //        BigInteger.Parse("57540880724084199423888850944000000") };

        //internal static DFloat GammaStirling( DFloat f, int maxBytes)
        //{
        //    if( f.IsZero ) throw new InvalidOperationException( "Gamma(0)" );

        //    // https://mathworld.wolfram.com/StirlingsSeries.html

        //    DFloat z = f.Trim( );

        //    // TODO: 


        //    throw new NotImplementedException();
        //}

        static readonly DFloat[] NemesCoef =
        [
            TryParse("0.08333333333333333333333333333333333 ", int.MaxValue)!,
            TryParse("0.00069444444444444444444444444444444 ", int.MaxValue)!,
            TryParse("0.00065861992945326278659611992945326 ", int.MaxValue)!,
            TryParse("-0.00053287817827748383303938859494415", int.MaxValue)!,
            TryParse("0.00079278588700608376534302460228386 ", int.MaxValue)!,
            TryParse("-0.00184758189322033028400606295961969", int.MaxValue)!,
            TryParse("0.00625067824784941846328836824623616 ", int.MaxValue)!,
            TryParse("-0.02901710246301150993444701506844402", int.MaxValue)!,
            TryParse("0.17718457242491308890302832366796470 ", int.MaxValue)!,
            TryParse("-1.37747681703993534399676348903067470", int.MaxValue)!,
        ];

        internal static DFloat GammaNemesPositive( DFloat f, int maxBytes )
        {
            if( f.IsZero ) throw new InvalidOperationException( "Gamma(0)" );
            if( f.IsNegative ) throw new InvalidOperationException( "GammaNemes(negative)" );

            // https://en.wikipedia.org/wiki/Stirling%27s_approximation, "Versions suitable for calculators"
            // http://www.ebyte.it/library/downloads/2007_MTH_Nemes_GammaFunction.pdf

            DFloat z = f.Trim( );

            DFloat s = z;
            int mb = maxBytes + 4;

            for( int i = 0; i < NemesCoef.Length; ++i )
            {
                DFloat c = NemesCoef[i];
                //int n = ( i + 1 ) * 2;
                int p = i * 2 + 1; // 1, 3, 5

                DFloat t = IntPow( z, p, false, mb );
                t = Div( c, t, mb );

                s = Add( s, t, mb );
            }

            s = Pow( s, z, mb );

            DFloat u = Div( Get2Pi( mb ), z, mb );
            u = SqrtHeron( u, mb );
            u = Mul( u, Pow( GetE( mb ), Neg( z ), mb ), mb );

            DFloat r = Mul( u, s, maxBytes );

            return r.Keep( 8 ).AsApprox( );
        }

        internal static DFloat GammaNegativeOnNemes( DFloat f, int maxBytes )
        {
            if( f.IsZero ) throw new InvalidOperationException( "Gamma(0)" );
            if( !f.IsNegative ) throw new InvalidOperationException( );

            DFloat x = f.Trim( );

            if( x.E >= 0 ) throw new InvalidOperationException( "Gamma for negative integer" );

            // https://mathworld.wolfram.com/GammaFunction.html
            // G(x)G(-x) = -π / (z sin(π x))

            int mb = maxBytes + 4;

            DFloat s = Sin( Mul( GetPi( mb ), x, mb ), mb ); // sin(π x)
            DFloat d = Mul( x, s, mb ); // z sin(π x)
            DFloat g = GammaNemesPositive( Neg( x ), mb ); // G(-x)

            DFloat t = Div( GetPi( mb ), d, mb ); // π / ( z sin( π x ))
            t = Div( t, g, mb );
            t = Neg( t );

            return t;
        }

        #endregion

        #region Trigonometry

        public static DFloat Sin1( DFloat f, int maxBytes, bool redu = true )
        {
            DFloat x = f.Trim( );

            // TODO: special values (π etc.)


            DFloat a = x.IsNegative ? Neg( x ) : x;
            int mb = maxBytes + 4;

            DFloat c2pi = Get2Pi( mb );

            if( redu )
            {
                if( a.CompareTo( c2pi ) > 0 )
                {
                    // reduce large values

                    DFloat d = Div( a, c2pi, mb );
                    DFloat t = Truncate( d );
                    t = Mul( t, c2pi, mb );

                    a = Sub( a, t, mb );
                }
            }

            DFloat eps = EvalEps( mb );
            DFloat fact = One;
            bool neg = true;
            DFloat r = a;

            for( int i = 3; ; i += 2, neg = !neg )
            {
                DFloat p = IntPow( a, i, false, mb );
                fact = Mul( fact, new DFloat( i * ( i - 1 ) ), mb );

                DFloat d = Div( p, fact, mb );

                r = Add( r, neg ? Neg( d ) : d, mb );

                if( d.CompareTo( eps ) < 0 ) break;
            }

            r = r.Reduce( maxBytes );

            return x.IsNegative ? Neg( r ) : r;
        }

        /// <summary>
        /// Sine.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat Sin( DFloat f, int maxBytes ) //....................
        {
            if( f.IsZero ) return f;

            // https://en.wikipedia.org/wiki/Sine_and_cosine, "Series and polynomials"

            DFloat x = f.Trim( );

            // TODO: special values (π etc.)

            DFloat a = x.IsNegative ? Neg( x ) : x;
            int mb = maxBytes + 4;

            if( a.CompareTo( Ten ) > 0 )
            {
                // reduce large values; 
                // (for very large values, the result is incorrect without such reduction);
                // use large π for more exact results

                a = Remainder( a, Get2Pi( int.MaxValue ), mb );
            }

            DFloat eps = EvalEps( mb );
            DFloat fact = One;
            bool neg = true;
            DFloat r = a;
            DFloat p = a;

            for( long i = 3; ; i += 2, neg = !neg )
            {
                p = Mul( a, Mul( a, p, mb ), mb );

                fact = Mul( fact, new DFloat( i * ( i - 1 ) ), mb );

                DFloat d = Div( p, fact, mb );

                r = Add( r, neg ? Neg( d ) : d, mb );

                if( d.CompareTo( eps ) < 0 ) break;
            }

            r = r.Reduce( maxBytes ).AsApprox( );

            return x.IsNegative ? Neg( r ) : r;
        }

        /// <summary>
        /// Cosine.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat Cos( DFloat f, int maxBytes )
        {
            if( f.IsZero ) return One.UnionApprox( f );

            // https://en.wikipedia.org/wiki/Sine_and_cosine, "Series and polynomials"

            DFloat x = f.Trim( );

            // TODO: special values (π etc.)

            DFloat a = Abs( x );
            int mb = maxBytes + 4;

            if( a.CompareTo( Ten ) > 0 )
            {
                // reduce large values; 
                // (for very large values, the result is incorrect without such reduction);
                // use large π for more exact results

                a = Remainder( a, Get2Pi( int.MaxValue ), mb );
            }

            DFloat eps = EvalEps( mb );
            DFloat fact = One;
            bool neg = true;
            DFloat r = One;
            DFloat p = One;

            for( long i = 2; ; i += 2, neg = !neg )
            {
                p = Mul( a, Mul( a, p, mb ), mb );

                fact = Mul( fact, new DFloat( i * ( i - 1 ) ), mb );

                DFloat d = Div( p, fact, mb );

                r = Add( r, neg ? Neg( d ) : d, mb );

                if( d.CompareTo( eps ) < 0 ) break;
            }

            r = r.Reduce( maxBytes ).AsApprox( );

            return r;
        }

        /// <summary>
        /// Tangent.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat Tg( DFloat f, int maxBytes )
        {
            if( f.IsZero ) return f;

            int mb = maxBytes + 4;

            return Div( Sin( f, mb ), Cos( f, mb ), maxBytes );
        }

        /// <summary>
        /// Cotangent.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat Ctg( DFloat f, int maxBytes )
        {
            if( f.IsZero ) throw new InvalidOperationException( "Ctg(0)" );

            int mb = maxBytes + 4;

            return Div( Cos( f, mb ), Sin( f, mb ), maxBytes );
        }

        /// <summary>
        /// ArcSine based on Taylor's series.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat ArcSinTaylorSeries( DFloat f, int maxBytes )
        {
            // Does not converge rapidly when 'f' is close to 1 or -1.

            if( f.CompareTo( MinusOne ) < 0 || f.CompareTo( One ) > 0 )
            {
                throw new InvalidOperationException( $"Invalid argument for {nameof( ArcSinTaylorSeries )}." ); // TODO: show value?
            }

            // https://en.wikipedia.org/wiki/Inverse_trigonometric_functions, "Infinite_series"

            DFloat z = f.Trim( );
            int mb = maxBytes + 4;
            DFloat eps = EvalEps( mb );

            DFloat n1 = One;
            DFloat n2 = Mul( Mul( z, z, mb ), z, mb ); // z**3
            DFloat d1 = Two;
            DFloat d2 = new( 3 );
            DFloat fr = Half;

            DFloat t =
                Div(
                    Mul( fr, n2, mb ),
                    d2, mb );

            DFloat r = Add( z, t, mb );

            for(; ; )
            {
                n1 = Add( n1, Two, mb );
                n2 = Mul( Mul( n2, z, mb ), z, mb );
                d1 = Add( d1, Two, mb );
                d2 = Add( d2, Two, mb );
                fr = Div( Mul( fr, n1, mb ), d1, mb );
                t = Div( Mul( fr, n2, mb ), d2, mb );

                r = Add( r, t, mb );

                if( Abs( t ).CompareTo( eps ) < 0 ) break;
            }

            r = r.Reduce( maxBytes ).AsApprox( );

            return r;
        }

        /// <summary>
        /// ArcSine based on Continued Fractions.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat ArcSinContinuedFraction( DFloat f, int maxBytes )
        {
            // Does not converge rapidly when 'f' is close to 1 or -1.

            // https://functions.wolfram.com/ElementaryFunctions/ArcSin/10/
            //
            // Continued fraction: f(x) = b0 + a1 / (b1 + a2 / (b2 + a3 / (b3 + a4 / (b4 + a5 / (b5 + ···

            DFloat z = f.Trim( );
            int mb = maxBytes + 4;
            DFloat zp2 = Mul( z, z, mb );
            DFloat eps = EvalEps( mb );
            DFloat tiny = Mul( eps, new DFloat( BigInteger.One, -20 ), mb ); // must be less than eps

            DFloat r = ThompsonBarnettModifiedLentz( eps, tiny, mb, GetA, GetB );

            return r.Reduce( maxBytes ).AsApprox( );

            DFloat GetA( int j, int mb )
            {
                Debug.Assert( j >= 1 );

                if( j == 1 )
                {
                    return Mul( z, SqrtHeron( Sub( One, zp2, mb ), mb ), mb );
                }
                else
                {
                    int k = j / 2 * 2 - 1;

                    return Mul( Mul( new DFloat( -k ), new DFloat( k + 1 ), mb ), zp2, mb );
                }
            }

            static DFloat GetB( int j, int mb )
            {
                Debug.Assert( j >= 0 );

                if( j == 0 )
                {
                    return Zero;
                }
                else
                {
                    int k = j * 2 - 1;

                    return new DFloat( k );
                }
            }
        }

        /// <summary>
        /// ArcSine based on arctg.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat ArcSinViaArcTan( DFloat f, int maxBytes )
        {
            if( f.CompareTo( MinusOne ) < 0 || f.CompareTo( One ) > 0 )
            {
                throw new InvalidOperationException( $"Invalid argument for {nameof( ArcSinViaArcTan )}." ); // TODO: show value?
            }

            if( f.IsZero ) return Zero;
            if( f.Equals( One ) ) return GetPiOverTwo( maxBytes );
            if( f.Equals( MinusOne ) ) return Neg( GetPiOverTwo( maxBytes ) );

            // https://en.wikipedia.org/wiki/Inverse_trigonometric_functions#Relationships_among_the_inverse_trigonometric_functions
            // arcsin(x) = arctan(x/sqrt(1-x**2))

            int mb = maxBytes + 4;

            DFloat t = Div( f, SqrtHeron( Sub( One, Mul( f, f, mb ), mb ), mb ), mb );
            DFloat r = ArcTanNewtonAccelerated( t, maxBytes );

            return r;
        }

        /// <summary>
        /// ArcCosine based on arctg.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat ArcCosViaArcTan( DFloat f, int maxBytes )
        {
            if( f.CompareTo( MinusOne ) < 0 || f.CompareTo( One ) > 0 )
            {
                throw new InvalidOperationException( $"Invalid argument for {nameof( ArcCosViaArcTan )}." ); // TODO: show value?
            }

            if( f.IsZero ) return GetPiOverTwo( maxBytes );
            if( f.Equals( One ) ) return Zero;
            if( f.Equals( MinusOne ) ) return GetPi( maxBytes );

            // https://en.wikipedia.org/wiki/Inverse_trigonometric_functions#Relationships_among_the_inverse_trigonometric_functions
            // arccos(x) = arctan(sqrt(1-x**2)/x)

            int mb = maxBytes + 4;

            DFloat t = Div( SqrtHeron( Sub( One, Mul( f, f, mb ), mb ), mb ), f, mb );
            DFloat r = ArcTanNewtonAccelerated( t, maxBytes );

            return r;
        }

        /// <summary>
        /// ArcTangent base of Taylor's series.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat ArcTanTaylorSeries( DFloat f, int maxBytes )
        {
            // https://en.wikipedia.org/wiki/Inverse_trigonometric_functions, "Infinite_series"

            DFloat z = f.Trim( );
            int mb = maxBytes + 4;
            DFloat eps = EvalEps( mb );

            DFloat r = z;
            DFloat n = z;
            DFloat d = One;
            bool neg = false;

            for(; ; )
            {
                neg = !neg;
                n = Mul( Mul( n, z, mb ), z, mb );  // TODO: use z**2?
                d = Add( d, Two, mb );
                DFloat t = Div( n, d, mb );

                if( neg )
                {
                    r = Sub( r, t, mb );
                }
                else
                {
                    r = Add( r, t, mb );
                }

                if( Abs( t ).CompareTo( eps ) < 0 ) break;
            }

            r = r.Reduce( maxBytes ).AsApprox( );

            return r;
        }

        /// <summary>
        /// ArcTangent based on Euler's series.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat ArcTanEulerSeries( DFloat f, int maxBytes )
        {
            if( f.IsZero ) return f;
            if( f.Equals( One ) ) return GetPiOverFour( maxBytes );
            if( f.Equals( MinusOne ) ) return Neg( GetPiOverFour( maxBytes ) );

            // https://en.wikipedia.org/wiki/Inverse_trigonometric_functions, "Infinite_series"
            // https://www.cambridge.org/core/journals/mathematical-gazette/article/abs/8967-an-elementary-derivation-of-eulers-series-for-the-arctangent-function/3D53E3BC23BEAF049A9F1013E89E9F46

            DFloat z = f.Trim( );
            int mb = maxBytes + 4;
            DFloat eps = EvalEps( mb );

            DFloat zp2 = Mul( z, z, mb ); // z**2
            DFloat t1 = Add( One, zp2, mb ); // 1 + z**2
            DFloat t2 = Div( Mul( Two, zp2, mb ), t1, mb );

            DFloat p = One;
            DFloat sum = p;

            for( long n = 1; ; ++n )
            {
                long k = n;

                DFloat t = Div(
                    Mul( t2, new DFloat( k ), mb ),
                    new DFloat( 2 * k + 1 ), mb );

                p = Mul( p, t, mb ); ;

                sum = Add( sum, p, mb );

                if( Abs( p ).CompareTo( eps ) < 0 ) break;
            }

            DFloat r = Div( Mul( z, sum, mb ), t1, maxBytes );

            return r;
        }

        /// <summary>
        /// ArcTangent based on Newton's method.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat ArcTanNewtonAccelerated( DFloat f, int maxBytes )
        {
            if( f.IsZero ) return f;
            if( f.Equals( One ) ) return GetPiOverFour( maxBytes );
            if( f.Equals( MinusOne ) ) return Neg( GetPiOverFour( maxBytes ) );

            // https://en.wikipedia.org/wiki/Arctangent_series, "Accelerated series"
            // https://en.wikipedia.org/wiki/Inverse_trigonometric_functions, "Infinite_series"

            DFloat z = Abs( f );
            DFloat r;

            if( z.CompareTo( One ) > 0 )
            {
                // arctan(1/x) = pi/2 - arctan(x), if x > 0

                z = Inv( z, maxBytes + 4 );
                r = ArcTanNewtonAcceleratedInternal( z, maxBytes + 4 );
                r = Sub( GetPiOverTwo( maxBytes + 4 ), r, maxBytes );
            }
            else
            {
                r = ArcTanNewtonAcceleratedInternal( z, maxBytes );
            }

            if( f.IsNegative ) r = Neg( r );

            return r.UnionApprox( f );

            static DFloat ArcTanNewtonAcceleratedInternal( DFloat f, int maxBytes )
            {
                DFloat x = f.Trim( );
                int mb = maxBytes + 4;
                DFloat eps = EvalEps( mb );

                //DFloat t1 = SqrtHeron( Add( One, Mul( x, x, mb ), mb ), mb ); // sqrt(1 + x**2)
                DFloat t1 = SqrtBinarySearch( Add( One, Mul( x, x, mb ), mb ), mb ); // sqrt(1 + x**2)
                DFloat C = Inv( t1, mb );
                DFloat S = Div( x, t1, mb );
                DFloat sum = S;
                DFloat n = One;
                DFloat d = One;
                DFloat ps = S;

                for( int i = 2; ; i += 2 )
                {
                    n = Mul( n, new DFloat( i ), mb );
                    d = Mul( d, new DFloat( i + 1 ), mb );
                    ps = Mul( Mul( ps, S, mb ), S, mb );

                    DFloat t = Div( Mul( n, ps, mb ), d, mb );
                    sum = Add( sum, t, mb );

                    if( Abs( t ).CompareTo( eps ) < 0 ) break;
                }

                DFloat r = Mul( C, sum, maxBytes );

                return r;
            }
        }

        /// <summary>
        /// ArcCotangent based on Newton's method.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat ArcCotNewtonAccelerated( DFloat f, int maxBytes )
        {
            if( f.IsZero ) return f;
            if( f.Equals( One ) ) return GetPiOverFour( maxBytes );
            if( f.Equals( MinusOne ) ) return Neg( GetPiOverFour( maxBytes ) );

            // https://en.wikipedia.org/wiki/Inverse_trigonometric_functions
            // arccot(x) = arctan(1/x) = pi/2 - arctan(x), if x > 0
            // arccot(-x) = pi - arccot(x)

            DFloat z = Abs( f );
            int mb = maxBytes + 2;
            DFloat r = ArcTanNewtonAccelerated( Inv( z, mb ), mb );

            if( f.IsNegative )
            {
                r = Sub( GetPi( mb ), r, maxBytes );

                // WolframAlpha appears to use "arccot(-x) = -arccot(x)"
            }
            else
            {
                r = r.Reduce( maxBytes );
            }

            return r;
        }

        #endregion

        #region Hyperbolics

        /// <summary>
        /// Hypherbolic Sine.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat Sinh( DFloat f, int maxBytes )
        {
            // https://en.wikipedia.org/wiki/Hyperbolic_functions, "Exponential definitions"

            if( f.IsZero ) return f;

            int mb = maxBytes + 4;

            DFloat exp = Exp( f, mb );

            return Div( Sub( exp, Inv( exp, mb ), mb ), Two, maxBytes );
        }

        /// <summary>
        /// Hypherbolic Cosine.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat Cosh( DFloat f, int maxBytes )
        {
            // https://en.wikipedia.org/wiki/Hyperbolic_functions, "Exponential definitions"

            if( f.IsZero ) return One.UnionApprox( f );

            int mb = maxBytes + 4;

            DFloat exp = Exp( f, mb );

            return Div( Add( exp, Inv( exp, mb ), mb ), Two, maxBytes );
        }

        /// <summary>
        /// Hypherbolic Tangent.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat Tanh( DFloat f, int maxBytes )
        {
            // https://en.wikipedia.org/wiki/Hyperbolic_functions, "Exponential definitions"

            if( f.IsZero ) return f;

            int mb = maxBytes + 4;

            return Div( Sinh( f, mb ), Cosh( f, mb ), maxBytes );
        }

        /// <summary>
        /// Hypherbolic Cotangent.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat Coth( DFloat f, int maxBytes )
        {
            // https://en.wikipedia.org/wiki/Hyperbolic_functions, "Exponential definitions"

            if( f.IsZero ) throw new InvalidOperationException( "Coth(0)." );

            int mb = maxBytes + 4;

            return Div( Cosh( f, mb ), Sinh( f, mb ), maxBytes );
        }

        /// <summary>
        /// Hypherbolic ArSinh.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat ArSinh( DFloat f, int maxBytes )
        {
            // https://en.wikipedia.org/wiki/Inverse_hyperbolic_functions, "Definitions in terms of logarithms"

            if( f.IsZero ) return f;

            int mb = maxBytes + 4;

            return Ln( Add( f, SqrtHeron( Add( Mul( f, f, mb ), One, mb ), mb ), mb ), maxBytes );
        }

        /// <summary>
        /// Hypherbolic ArCosh.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat ArCosh( DFloat f, int maxBytes )
        {
            // https://en.wikipedia.org/wiki/Inverse_hyperbolic_functions, "Definitions in terms of logarithms"

            switch( f.CompareTo( One ) )
            {
            case < 0: // f < 1
                throw new InvalidOperationException( "ArCosh of value less than 1." );
            case 0: // f == 1
                return Zero.UnionApprox( f );
            }

            int mb = maxBytes + 4;

            return Ln( Add( f, SqrtHeron( Add( Mul( f, f, mb ), MinusOne, mb ), mb ), mb ), maxBytes );
        }

        /// <summary>
        /// Hypherbolic ArTangent.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat ArTanh( DFloat f, int maxBytes )
        {
            // https://en.wikipedia.org/wiki/Inverse_hyperbolic_functions, "Definitions in terms of logarithms"

            switch( f.CompareTo( MinusOne ) )
            {
            case <= 0: // f <= -1
                throw new InvalidOperationException( "ArTanh of value which is <= -1." );
            }

            switch( f.CompareTo( One ) )
            {
            case >= 0: // f >= 1
                throw new InvalidOperationException( "ArTanh of value which is >= 1." );
            }

            int mb = maxBytes + 4;

            return Mul( Half, Ln( Div( Add( One, f, mb ), Sub( One, f, mb ), mb ), mb ), maxBytes );
        }

        /// <summary>
        /// Hypherbolic ArCotangent.
        /// </summary>
        /// <param name="f"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        public static DFloat ArCoth( DFloat f, int maxBytes )
        {
            // https://en.wikipedia.org/wiki/Inverse_hyperbolic_functions, "Definitions in terms of logarithms"

            switch( f.CompareTo( MinusOne ) )
            {
            case >= 0: // f >= -1
                switch( f.CompareTo( One ) )
                {
                case <= 0: // f <= 1
                    throw new InvalidOperationException( "ArCoth of value which is between -1 and +1." );
                }
                break;
            }

            int mb = maxBytes + 4;

            return Mul( Half, Ln( Div( Add( f, One, mb ), Sub( f, One, mb ), mb ), mb ), maxBytes );
        }

        #endregion

        #region Parsing and Conversions

        /// <summary>
        /// Parse a string. Format: "[+/-]DDD[.DDD]e[+/-]DDD".
        /// </summary>
        /// <param name="text"></param>
        /// <param name="maxBytes"></param>
        /// <returns></returns>
        public static DFloat? TryParse( string text, int maxBytes )
        {
            return DFloatFormatter.TryParse( text, maxBytes );
        }

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
