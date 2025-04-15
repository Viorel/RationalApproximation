using System.Numerics;

namespace RationalApproximationLibrary
{
    // NOTE. Unused code is almost deleted.

    public class CalculationContext
    {
        int mMaxDigits;

        public ICancellable Cnc { get; }
        public int MaxDigits
        {
            get => mMaxDigits;
            set
            {
                if( mMaxDigits != value )
                {
                    mMaxDigits = value;
                    Recalculate( );
                }
            }
        }

        public BigInteger MinVal { get; private set; }
        public BigInteger MaxVal { get; private set; }
        public BigInteger MinValDiv10 { get; private set; }
        public BigInteger MaxValDiv10 { get; private set; }
        public int MaxBytes { get; private set; }


        public CalculationContext( ICancellable cnc, int maxDigits )
        {
            ArgumentOutOfRangeException.ThrowIfLessThan( maxDigits, 2 );

            Cnc = cnc;
            mMaxDigits = maxDigits;

            Recalculate( );
        }

        void Recalculate( )
        {
            MaxVal = BigInteger.Pow( 10, mMaxDigits ) - 1;
            MinVal = -MaxVal;
            MinValDiv10 = BigInteger.Divide( MinVal, 10 );
            MaxValDiv10 = BigInteger.Divide( MaxVal, 10 );
            MaxBytes = MaxVal.GetByteCount( );
        }
    }
}
