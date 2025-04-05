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
            return IsCancellationRequested ? throw new OperationCanceledException( ) : false;
        }

        public static ICancellable NonCancellable => RationalApproximationLibrary.NonCancellable.Instance;
    }

    sealed class NonCancellable : ICancellable
    {
        public static readonly ICancellable Instance = new NonCancellable( );

        #region ICancellable

        public bool IsCancellationRequested => false;

        #endregion ICancellable
    }

    public sealed class SimpleCancellable : ICancellable
    {
        bool mCancel = false;

        public void SetCancel( ) => mCancel = true;

        #region ICancellable

        public bool IsCancellationRequested => mCancel;

        #endregion ICancellable
    }
}
