using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;


namespace RationalApproximationLibrary
{
    public interface ICancellable
    {
        bool IsCancellationRequested { get; }

        bool TryThrow( ) // only returns false, or throws the exception
        {
            if( IsCancellationRequested )
            {
                throw new OperationCanceledException( );
            }

            return false;
        }

        public static ICancellable NonCancellable => RationalApproximationLibrary.NonCancellable.Instance;
    }


    public sealed class NonCancellable : ICancellable
    {
        public static readonly ICancellable Instance = new NonCancellable( );

        #region ICancellable

        public bool IsCancellationRequested
        {
            get
            {
                return false;
            }
        }

        #endregion ICancellable
    }
}
