using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Numerics;
using System.Text;
using System.Text.RegularExpressions;

namespace RationalApproximationLibrary
{
    partial class FractionFormatter
    {
        static readonly BigInteger Bi10 = 10;
        static readonly BigInteger Bi5 = 5;
        static readonly NumberFormatInfo CustomNFI = BuildCustomNFI( );

        internal static Fraction? TryParse( string s )
        {
            Regex re = ParserRegex( );

            Match m = re.Match( s );

            if( !m.Success ) return null;

            if( m.Groups["I"].Success ) return Fraction.PositiveInfinity;
            if( m.Groups["NI"].Success ) return Fraction.NegativeInfinity;
            if( m.Groups["U"].Success ) return Fraction.Undefined;

            bool is_negative = m.Groups["NS"].Value == "-";

            BigInteger n = BigInteger.Parse( m.Groups["N"].Value.Replace( "_", "", StringComparison.InvariantCulture ).Replace( "\u2009", "", StringComparison.InvariantCulture ), CultureInfo.InvariantCulture );
            BigInteger e;

            Group eg = m.Groups["NE"];

            if( eg.Success )
            {
                string e_s = eg.Value.Replace( "_", "", StringComparison.InvariantCulture ).Replace( "\u2009", "", StringComparison.InvariantCulture );

                e = BigInteger.Parse( e_s, CultureInfo.InvariantCulture );
            }
            else
            {
                e = BigInteger.Zero;
            }

            Group fg = m.Groups["F"];
            Group rg = m.Groups["R"];

            if( rg.Success ) // 123.(67), 123.45(67), maybe with eXXX
            {
                string f_s = fg.Success ? fg.Value : "";
                string r_s = rg.Value.Replace( "_", "", StringComparison.InvariantCulture ).Replace( "\u2009", "", StringComparison.InvariantCulture );

                int f_len = f_s.Length;
                int r_len = r_s.Length;

                Debug.Assert( r_len != 0 );

                BigInteger f;

                if( f_len == 0 )
                {
                    f = BigInteger.Zero;
                }
                else
                {
                    f = BigInteger.Parse( f_s, CultureInfo.InvariantCulture );
                }

                BigInteger r;

                r = BigInteger.Parse( r_s, CultureInfo.InvariantCulture );

                BigInteger fpt = BigInteger.Pow( Bi10, f_len );
                BigInteger rpt = BigInteger.Pow( Bi10, r_len );

                Debug.Assert( n >= 0 );

                BigInteger t2 = n * fpt + f;
                BigInteger t1 = t2 * rpt + r;
                BigInteger diff = t1 - t2;
                BigInteger d = fpt * ( rpt - 1 );

                Debug.Assert( diff >= 0 );

                return new Fraction( is_negative ? -diff : diff, d, e );
            }

            if( fg.Success )
            {
                // 123.45, maybe with eXXX

                BigInteger f;

                string f_s = fg.Value.Replace( "_", "", StringComparison.InvariantCulture ).Replace( "\u2009", "", StringComparison.InvariantCulture ).TrimEnd( '0' );

                if( f_s.Length == 0 )
                {
                    f = BigInteger.Zero;
                }
                else
                {
                    f = BigInteger.Parse( f_s, CultureInfo.InvariantCulture );
                }

                for( int i = 0; i < f_s.Length; ++i )
                {
                    n *= Bi10;
                }

                n += f;

                e -= f_s.Length;

                return new Fraction( is_negative ? -n : n, BigInteger.One, e );
            }
            else
            {
                // 123, 123/78, 123//78, maybe with eXXX after numerator and/or denominator

                Group dg = m.Groups["D"];

                if( dg.Success )
                {
                    BigInteger d = BigInteger.Parse( dg.Value.Replace( "_", "", StringComparison.InvariantCulture ).Replace( "\u2009", "", StringComparison.InvariantCulture ), CultureInfo.InvariantCulture );

                    BigInteger de;

                    Group deg = m.Groups["DE"];
                    if( deg.Success )
                    {
                        string de_s = deg.Value.Replace( "_", "", StringComparison.InvariantCulture ).Replace( "\u2009", "", StringComparison.InvariantCulture );

                        de = BigInteger.Parse( de_s, CultureInfo.InvariantCulture );
                    }
                    else
                    {
                        de = BigInteger.Zero;
                    }

                    return new Fraction( is_negative ? -n : n, d, e - de );
                }
                else
                {
                    return new Fraction( is_negative ? -n : n, BigInteger.One, e );
                }
            }
        }


        public static string ToFloatString( ICancellable cnc, Fraction f, int maxDigits, bool group = false )
        {
            if( maxDigits < 3 ) throw new ArgumentException( "Precision too small" );

            // Simplification using GCD is not done here; 

            if( f.IsUndefined ) return f.IsApprox ? "≈Undefined" : "Undefined";
            if( f.IsPositiveInfinity ) return f.IsApprox ? "≈+Infinity" : "+Infinity";
            if( f.IsNegativeInfinity ) return f.IsApprox ? "≈-Infinity" : "-Infinity";

            Debug.Assert( f.IsNormal );

            if( f.IsZero ) return f.IsApprox ? "≈0" : "0";

            StringBuilder sb = new( );

            FormatFloatingPoint( cnc, sb, f.N, f.D, f.E, maxDigits, group );

            if( f.IsApprox && sb[0] != '≈' )
            {
                sb.Insert( 0, '≈' );
            }

            return sb.ToString( );
        }


        static void FormatFloatingPoint( ICancellable cnc, StringBuilder sb, BigInteger n, BigInteger d, BigInteger e, int maxDigits, bool group = false )
        {
            // Simplification using GCD is not done here; 

            if( d <= 0 ) throw new ArgumentException( "Invalid denominator" );

            if( n == 0 )
            {
                sb.Append( '0' );

                return;
            }

            bool is_negative;

            if( n < 0 )
            {
                is_negative = true;
                n = -n;
            }
            else
            {
                is_negative = false;
            }

            (n, int en) = FractionUtilities.TrimZeroesGE0( cnc, n );
            (d, int ed) = FractionUtilities.TrimZeroesGE0( cnc, d );
            e += en - ed;

            {
                // make sure that 'n' in not shorter than 'd'

                int ndn = FractionUtilities.NumberOfDigitsGE0( cnc, n );
                int ndd = FractionUtilities.NumberOfDigitsGE0( cnc, d );

                if( ndn < ndd )
                {
                    int zeroes_to_add = ndd - ndn;

                    n *= BigInteger.Pow( Bi10, zeroes_to_add );
                    e -= zeroes_to_add;
                }
            }

            Debug.Assert( n > 0 );
            Debug.Assert( d > 0 );

            (BigInteger q, BigInteger r) = BigInteger.DivRem( n, d );

            int ndq = q == 0 ? 0 : FractionUtilities.NumberOfDigitsGE0( cnc, q );

            cnc.TryThrow( );

            if( ndq > maxDigits )
            {
                // too long integer part

                (q, BigInteger _) = BigInteger.DivRem( q, BigInteger.Pow( Bi10, ndq - maxDigits - 1 ) ); // include one more digit
                e += ndq - maxDigits - 1;

                cnc.TryThrow( );

                // round
                q += 5;
                ndq = FractionUtilities.NumberOfDigits( cnc, q );
                (q, BigInteger _) = BigInteger.DivRem( q, BigInteger.Pow( Bi10, ndq - maxDigits ) );
                Debug.Assert( FractionUtilities.NumberOfDigits( cnc, q ) == maxDigits );

                e += ndq - maxDigits;

                cnc.TryThrow( );

                // ≈ -- 'ALMOST EQUAL TO' (U+2248)
                // ≅ -- 'APPROXIMATELY EQUAL TO' (U+2245)

                sb.Append( '≈' );

                string int_part = q.ToString( "D", CultureInfo.InvariantCulture );

                FormatFloatingPointString( cnc, sb, is_negative, int_part, "", "", e, maxDigits, group );
            }
            else
            {
                // integer part not too long

                int allowed_space = maxDigits - ndq;
                Debug.Assert( allowed_space >= 0 );

                int ndr = FractionUtilities.NumberOfDigitsGE0( cnc, r );

                StringBuilder sbf = new( );

                Dictionary<BigInteger, int> remainders = [];

                int repeating_index = -1;
                string repeating_part = "";

                for( int i = 0; r != 0 && i < allowed_space; ++i )
                {
                    cnc.TryThrow( );
                    remainders.Add( r, 0 );

                    (BigInteger qf, r) = BigInteger.DivRem( r * Bi10, d );

                    Debug.Assert( qf >= 0 );
                    Debug.Assert( qf < 10 );

                    sbf.Append( qf.ToString( "D", CultureInfo.InvariantCulture ) );

                    if( remainders.TryGetValue( r, out repeating_index ) )
                    {
                        repeating_part = sbf.ToString( repeating_index, sbf.Length - repeating_index );
                        sbf.Length = repeating_index;

                        break;
                    }
                }

                string int_part = q.ToString( "D", CultureInfo.InvariantCulture );
                string float_part = sbf.ToString( );
                bool is_approx = repeating_part.Length == 0 && !r.IsZero;

                if( is_approx )
                {
                    // round

                    (BigInteger qf, _) = BigInteger.DivRem( r * Bi10, d ); // get one more digit
                    Debug.Assert( qf >= 0 );
                    Debug.Assert( qf < 10 );

                    if( qf >= Bi5 )
                    {
                        int float_part_len = float_part.Length;

                        if( float_part_len != 0 )
                        {
                            BigInteger float_part_num = BigInteger.Parse( float_part, CultureInfo.InvariantCulture );
                            float_part_num = float_part_num * Bi10 + qf + 5;
                            float_part_num /= Bi10;

                            float_part = float_part_num.ToString( "D", CultureInfo.InvariantCulture );
                            float_part = float_part.PadLeft( float_part_len, '0' );

                            Debug.Assert( float_part.Length == float_part_len || float_part.Length == float_part_len + 1 );

                            if( float_part.Length > float_part_len )
                            {
                                // overflow

                                Debug.Assert( float_part.StartsWith( '1' ) );

                                float_part = float_part[1..];

                                int int_part_len = int_part.Length;

                                ++q;

                                int_part = q.ToString( "D", CultureInfo.InvariantCulture );

                                Debug.Assert( int_part.Length == int_part_len || int_part.Length == int_part_len + 1 );

                                if( int_part.Length > int_part_len )
                                {
                                    float_part = int_part[^1] + float_part[0..^1];
                                    int_part = int_part[0..^1];

                                    ++e;
                                }

                                Debug.Assert( int_part.Length == int_part_len );
                            }
                        }
                        else
                        {
                            int int_part_len = int_part.Length;

                            ++q;

                            int_part = q.ToString( "D", CultureInfo.InvariantCulture );

                            Debug.Assert( int_part.Length == int_part_len || int_part.Length == int_part_len + 1 );

                            if( int_part.Length > int_part_len )
                            {
                                int_part = int_part[0..^1];

                                ++e;
                            }

                            Debug.Assert( int_part.Length == int_part_len );
                        }

                        Debug.Assert( float_part.Length == float_part_len );
                    }
                }

                if( is_approx ) sb.Append( '≈' );

                FormatFloatingPointString( cnc, sb, is_negative, int_part, float_part, repeating_part, e, maxDigits, group );
            }
        }

        internal static void FormatFloatingPointString( ICancellable cnc, StringBuilder sb, bool isNegative, string intPart, string floatPart, string repeatingPart, BigInteger e, int maxDigits, bool group )
        {
            intPart ??= "0";
            floatPart ??= "";
            repeatingPart ??= "";

            intPart = intPart.TrimStart( '0' );
            if( intPart.Length == 0 ) intPart = "0";

            if( intPart == "0" )
            {
                // try to borrow a non-zero digit from 'floatPart'

                string t = floatPart;
                floatPart = floatPart.TrimStart( '0' );

                e -= t.Length - floatPart.Length;

                if( floatPart.Length > 0 )
                {
                    intPart = floatPart[0..1];
                    floatPart = floatPart[1..];
                    e -= 1;
                }
            }
            else if( intPart.Length > 1 )
            {
                // keep only the first non-zero digit in 'int_part'

                e += intPart.Length - 1;
                floatPart = intPart[1..] + floatPart;
                intPart = intPart[0..1];
            }

            // trim unneeded zeroes
            if( repeatingPart.TrimEnd( '0' ).Length == 0 ) repeatingPart = "";
            if( repeatingPart.Length == 0 ) floatPart = floatPart.TrimEnd( '0' );

            cnc.TryThrow( );

            Debug.Assert( intPart.Length == 1 );
            Debug.Assert( intPart == "0" && floatPart.Length == 0 || intPart != "0" );

            if( ( intPart == "0" ? 0 : 1 ) + floatPart.Length + repeatingPart.Length > maxDigits ) throw new ApplicationException( "The number components are too large" );

            if( intPart == "0" && repeatingPart.Length == 0 )
            {
                // zero

                sb.Append( '0' );

                isNegative = false;
                e = BigInteger.Zero;
            }
            else
            {
                if( isNegative ) sb.Append( '-' );

                if( intPart.Length == 1 && intPart != "0" && e == -1 )
                {
                    // specific case:
                    //      1.2345e-1 ==> 0.12345
                    //      1e-1 ==> 0.1

                    (string newFloatPart, repeatingPart) = Optimise( intPart + floatPart, repeatingPart );

                    sb
                        .Append( "0." )
                        .Append( newFloatPart );

                    e = 0;
                }
                else if( e < 0 )
                {
                    if( -e >= maxDigits - ( floatPart.Length + repeatingPart.Length ) )
                    {
                        // e too large
                        // 1.23(456)e-200

                        Debug.Assert( intPart.Length == 1 );

                        sb.Append( intPart );

                        if( floatPart.Length > 0 || repeatingPart.Length > 0 )
                        {
                            sb
                                .Append( '.' )
                                .Append( floatPart );
                        }
                    }
                    else
                    {
                        // e not large
                        // 1.23(456)e-2 ==> 0.0123(456)

                        Debug.Assert( e < 0 );

                        (string newFloatPart, repeatingPart) = Optimise( new string( '0', -(int)e - 1 ) + intPart + floatPart, repeatingPart );

                        sb
                            .Append( "0." )
                            .Append( newFloatPart );

                        e = BigInteger.Zero;
                    }
                }
                else if( e.IsZero )
                {
                    // e is zero
                    // 12, 12.34, 12.34(5)

                    sb.Append( intPart );

                    if( group ) MakeGroups( sb, sb.Length - intPart.Length );

                    if( floatPart.Length > 0 || repeatingPart.Length > 0 )
                    {
                        sb
                            .Append( '.' )
                            .Append( floatPart );
                    }
                }
                else
                {
                    // e is positive 

                    Debug.Assert( e > 0 );
                    Debug.Assert( !( intPart == "0" && floatPart.Length > 0 ) );
                    Debug.Assert( intPart.Length == 1 );

                    if( e <= floatPart.Length )
                    {
                        // e not larger than float part
                        // 1.23(456)e+1 ==> 12.3(456)
                        // 1.23(456)e+2 ==> 123.(456)

                        int k = (int)e;
                        int pos = sb.Length;

                        sb
                            .Append( intPart )
                            .Append( floatPart[0..k] );

                        if( group ) MakeGroups( sb, pos );

                        string t = floatPart[k..];

                        if( t.Length > 0 || repeatingPart.Length > 0 )
                        {
                            sb
                                .Append( '.' )
                                .Append( t );
                        }

                        e = BigInteger.Zero;
                    }
                    else if( repeatingPart.Length == 0 && e < maxDigits )
                    {
                        // e not larger than allowed space, no repeating part

                        // 12.345e2 ==> 12345
                        // 12.345e5 ==> 1234500

                        int pos = sb.Length;

                        sb
                            .Append( intPart )
                            .Append( floatPart )
                            .Append( '0', (int)e - floatPart.Length );

                        if( group ) MakeGroups( sb, pos );

                        e = BigInteger.Zero;
                    }
                    else if( repeatingPart.Length != 0 && ( intPart == "0" ? e <= maxDigits - repeatingPart.Length : e < maxDigits - repeatingPart.Length ) )
                    {
                        // e larger than the float part and it is possible to take digits from repeating part

                        Debug.Assert( e > floatPart.Length );

                        int pos = sb.Length;

                        if( intPart != "0" || floatPart.Length > 0 )
                        {
                            sb
                                .Append( intPart )
                                .Append( floatPart );
                        }

                        Debug.Assert( e > 0 );

                        int je = 0;
                        int int_e = (int)e - floatPart.Length;
                        Debug.Assert( int_e > 0 );
                        for( int i = 0; i < int_e; ++i )
                        {
                            sb.Append( repeatingPart[je] );

                            je = ++je % repeatingPart.Length;
                        }
                        e = BigInteger.Zero;

                        if( group ) MakeGroups( sb, pos );

                        sb.Append( '.' );

                        repeatingPart = repeatingPart[je..] + repeatingPart[0..je];
                    }
                    else
                    {
                        // e is large
                        // 123.45e+100 ==> 1.2345e+102
                        // 123.45(7)e+100 ==> 1.2345(7)e+102

                        sb.Append( intPart );

                        if( group ) MakeGroups( sb, sb.Length - intPart.Length );

                        if( floatPart.Length > 0 || repeatingPart.Length > 0 )
                        {
                            sb
                                .Append( '.' )
                                .Append( floatPart );
                        }
                    }
                }

                cnc.TryThrow( );

                // append repeating part

                if( repeatingPart.Length > 0 )
                {
                    sb
                        .Append( '(' )
                        .Append( repeatingPart )
                        .Append( ')' );
                }

                cnc.TryThrow( );

                // append 'e'

                if( !e.IsZero )
                {
                    sb
                        .Append( 'e' )
                        .Append( e.ToString( "+0;-0", CultureInfo.InvariantCulture ) );
                }
            }

            Debug.Assert( isNegative && sb.Length > 1 || !isNegative && sb.Length > 0 );
        }

        static void MakeGroups( StringBuilder sb, int startIndex )
        {
            for( int i = sb.Length - 3; i > startIndex; i -= 3 )
            {
                sb.Insert( i, '\u2009' );
            }
        }

        static (string newFloatPart, string newRepeatingPart) Optimise( string floatPart, string repeatingPart )
        {
            if( floatPart.Length == 0 || repeatingPart.Length == 0 )
            {
                return (floatPart, repeatingPart);
            }

            while( floatPart.Length > 0 && floatPart[^1] == repeatingPart[^1] )
            {
                floatPart = floatPart[..^1];
                repeatingPart = repeatingPart[^1] + repeatingPart[..^1];
            }

            return (floatPart, repeatingPart);
        }


        public static string ToRationalString( ICancellable cnc, Fraction f, int maxDigits )
        {
            if( maxDigits < 3 ) throw new ArgumentException( "Precision too small" );

            // Simplification using GCD is not done here; 

            if( f.IsUndefined ) return f.IsApprox ? "≈Undefined" : "Undefined";
            if( f.IsPositiveInfinity ) return f.IsApprox ? "≈+Infinity" : "+Infinity";
            if( f.IsNegativeInfinity ) return f.IsApprox ? "≈-Infinity" : "-Infinity";

            Debug.Assert( f.IsNormal );

            if( f.IsZero ) return f.IsApprox ? "≈0" : "0";

            (BigInteger n, BigInteger e_n) = FractionUtilities.TrimZeroes( cnc, f.N );
            (BigInteger d, BigInteger e_d) = FractionUtilities.TrimZeroesGE0( cnc, f.D );

            BigInteger e = f.E + ( e_n - e_d );

            (bool n_is_approx, n, e_n) = ApplyMaxDigits( cnc, n, maxDigits );
            e += e_n;
            (bool d_is_approx, d, e_d) = ApplyMaxDigits( cnc, d, maxDigits );
            e -= e_d;

            bool is_approx = f.IsApprox || n_is_approx || d_is_approx;

            StringBuilder sb = new( );

            // ≈ -- 'ALMOST EQUAL TO' (U+2248)
            // ≅ -- 'APPROXIMATELY EQUAL TO' (U+2245)

            if( is_approx ) sb.Append( '≈' );

            int ndn = FractionUtilities.NumberOfDigits( cnc, n );
            int ndd = FractionUtilities.NumberOfDigitsGE0( cnc, d );

            if( e >= 0 )
            {
                if( ndn + e <= maxDigits )
                {
                    // 123e2 ==> 12300

                    n *= BigInteger.Pow( Bi10, (int)e );

                    sb.Append( n.ToString( "#,##0", CustomNFI ) );
                }
                else
                {
                    // 123e1000

                    sb
                        .Append( n.ToString( "#,##0", CustomNFI ) )
                        .Append( 'e' )
                        .Append( e.ToString( "+0;-0", CustomNFI ) );
                }
            }
            else
            {
                Debug.Assert( e < 0 );

                if( ndd + -e <= maxDigits )
                {
                    sb.Append( n.ToString( "#,##0", CustomNFI ) );

                    // move zeroes to 'd'
                    d *= BigInteger.Pow( Bi10, -(int)e );
                }
                else
                {
                    sb
                        .Append( n.ToString( "#,##0", CustomNFI ) )
                        .Append( 'e' )
                        .Append( e.ToString( "+0;-0", CustomNFI ) );
                }
            }

            Debug.Assert( d >= 1 );

            if( d > 1 )
            {
                sb
                    .Append( '/' )
                    .Append( d.ToString( "#,##0", CustomNFI ) );
            }

            return sb.ToString( );
        }


        internal static (bool isApprox, BigInteger n, int e) ApplyMaxDigits( ICancellable cnc, BigInteger n, int maxDigits )
        {
            Debug.Assert( maxDigits > 0 );

            (n, int e) = FractionUtilities.TrimZeroes( cnc, n );

            int ndn = FractionUtilities.NumberOfDigits( cnc, n );

            if( ndn <= maxDigits ) return (false, n, e); // the number is smaller

            int digits_to_cut = ndn - maxDigits;

            Debug.Assert( digits_to_cut > 0 );

            BigInteger pw = BigInteger.Pow( Bi10, digits_to_cut );

            (BigInteger q, BigInteger r) = BigInteger.DivRem( n, pw );
            // (NOTE. If 'n' is negative, then 'q' and 'r' are negative)

            cnc.TryThrow( );

            if( r < 0 ) r = -r;
            bool is_approx = !r.IsZero;
            e += digits_to_cut;

            if( r >= pw / 2 )
            {
                // "Rounding half away from zero": 123.5 ==> 124, -123.5 ==> -124

                if( q >= 0 ) // (actually never 'q == 0')
                {
                    q += 1;
                }
                else
                {
                    q -= 1;
                }
            }

            // for example, if '999 + 1' becomes '1000', then keep '1e3'

            (q, int e2) = FractionUtilities.TrimZeroes( cnc, q );
            e += e2;

            return (is_approx, q, e);
        }

        static NumberFormatInfo BuildCustomNFI( )
        {
            var nfi = (NumberFormatInfo)CultureInfo.InvariantCulture.NumberFormat.Clone( );
            nfi.NumberGroupSeparator = "\u2009";

            return nfi;
        }


        [StringSyntax( StringSyntaxAttribute.Regex )]
        const string ParserPattern = @"
(?nxi) ^\s*
(
  (?<I>\+?(infinity|∞)) | 
  (?<NI>-(infinity|∞)) | 
  (?<U>undefined) | 
  ((?<NS>[+\-])? (?<N>\d+([_\u2009]\d+)*) ([.] (?<F>\d+([_\u2009]\d+)*)? (\((?<R>\d+([_\u2009]\d+)*)\))? (?(F)|(?(R)|(?!))) )? ([e](?<NE>[+\-]?\d+([_\u2009]\d+)*))? 
    (?(F) | (\s* //? \s* (?<D>\d+([_\u2009]\d+)*) )? ([e](?<DE>[+\-]?\d+([_\u2009]\d+)*))? ) )
)
\s*$
";

        [GeneratedRegex( ParserPattern, RegexOptions.IgnorePatternWhitespace )]
        private static partial Regex ParserRegex( );
    }
}
