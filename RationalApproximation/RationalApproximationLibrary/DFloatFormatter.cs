using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace RationalApproximationLibrary
{
    internal partial class DFloatFormatter
    {
        static readonly BigInteger Bi10 = 10;

        public static DFloat? TryParse( string text, int maxBytes )
        {
            Match m = RegexParse( ).Match( text );
            if( !m.Success ) return null;

            Group gn = m.Groups["n"];
            Group gi = m.Groups["i"];
            Group gf = m.Groups["f"];
            Group gr = m.Groups["r"];
            Group gne = m.Groups["ne"];
            Group ge = m.Groups["e"];

            BigInteger e = BigInteger.Zero;
            if( ge.Success )
            {
                e = BigInteger.Parse( ge.Value );
                if( gne.Success ) e = -e;
            }

            string nom_s = gi.Value;

            if( gf.Success )
            {
                nom_s += gf.Value;
                e -= gf.Value.Length;
            }

            BigInteger nom = BigInteger.Parse( nom_s );

            if( gn.Success ) nom = -nom;

            if( gr.Success && nom.GetByteCount( ) < maxBytes + 1 )
            {
                BigInteger r = BigInteger.Parse( gr.Value );
                int er = gr.Value.Length;
                BigInteger pr = BigInteger.Pow( Bi10, er );

                do
                {
                    nom = nom * pr + r;
                    e -= er;
                } while( nom.GetByteCount( ) < maxBytes + 1 );
            }

            return new DFloat( nom, e ).Reduce( maxBytes );
        }


        [GeneratedRegex( @"(?nxi) ^\s*
(\+|(?<n>-))? 0* (?<i> \d+)
(\.
 ( 0+ | (?<f> \d+?) 0* | (?<f> \d+)? (\( (?<r> \d+) \)) )
)?
(e (\+|(?<ne>-))? (0+ | 0* (?<e> \d+)) )?
\s*$
", RegexOptions.None, cultureName: "" )]
        private static partial Regex RegexParse( );

    }
}
