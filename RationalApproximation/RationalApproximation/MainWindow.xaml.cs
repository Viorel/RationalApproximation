using System.Diagnostics;
using System.Globalization;
using System.Numerics;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Windows.Threading;
using Microsoft.Win32;
using RationalApproximationLibrary;

namespace RationalApproximation
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        const int MAX_MAXDIGITS = 1000;
        readonly TimeSpan DELAY_BEFORE_CALCULATION = TimeSpan.FromMilliseconds( 222 );
        readonly TimeSpan DELAY_BEFORE_PROGRESS = TimeSpan.FromMilliseconds( 222 );
        readonly TimeSpan MIN_DURATION_PROGRESS = TimeSpan.FromMilliseconds( 333 );

        bool mLoaded = false;
        bool mIsRestoreError = false;
        readonly DispatcherTimer mCalculationTimer;
        Thread? mCalculationThread = null;
        SimpleCancellable? mLastCancellable = null;
        readonly DispatcherTimer mProgressTimer = new( );
        DateTime mProgressShownTime = DateTime.MinValue;
        enum ProgressStatusEnum { None, DelayToShow, DelayToHide };
        ProgressStatusEnum mProgressStatus = ProgressStatusEnum.None;

        public MainWindow( )
        {
            InitializeComponent( );

            richTextBoxNote.Visibility = Visibility.Visible;
            richTextBoxTypicalError.Visibility = Visibility.Hidden;
            richTextBoxError.Visibility = Visibility.Hidden;
            richTextBoxResult.Visibility = Visibility.Hidden;
            labelPleaseWait.Visibility = Visibility.Hidden;

            mCalculationTimer = new DispatcherTimer
            {
                Interval = DELAY_BEFORE_CALCULATION,
            };
            mCalculationTimer.Tick += CalculationTimer_Tick;

            mProgressTimer.Tick += ProgressTimer_Tick;
        }

        private void Window_SourceInitialized( object sender, EventArgs e )
        {
            try
            {
                RestoreWindowPlacement( );
                RestoreMaximizedState( );
            }
            catch( Exception exc )
            {
                mIsRestoreError = true;

                if( Debugger.IsAttached ) Debugger.Break( );
                else Debug.Fail( exc.Message, exc.ToString( ) );
            }
        }

        private void Window_Loaded( object sender, RoutedEventArgs e )
        {
            mLoaded = true;

            ApplySavedData( );

            textBoxInput.Focus( );
            textBoxInput.SelectAll( );

            //RestartCalculations( );
        }

        private void Window_Closing( object sender, System.ComponentModel.CancelEventArgs e )
        {
            mCalculationTimer.Stop( );
            StopThread( );

            if( !mIsRestoreError ) // avoid overwriting details in case of errors
            {
                try
                {
                    SaveData( );
                    SaveWindowPlacement( );
                }
                catch( Exception exc )
                {
                    if( Debugger.IsAttached ) Debugger.Break( );
                    else Debug.Fail( exc.Message, exc.ToString( ) );
                }
            }
        }

        private void textBoxInput_TextChanged( object sender, TextChangedEventArgs e )
        {
            if( !mLoaded ) return;

            RestartCalculationTimer( );
        }

        private void comboBoxDigits_TextChanged( object sender, TextChangedEventArgs e )
        {
            if( !mLoaded ) return;

            RestartCalculationTimer( );
        }

        private void CalculationTimer_Tick( object? sender, EventArgs e )
        {
            mCalculationTimer.Stop( );

            RestartCalculations( );
        }

        void RestartCalculationTimer( )
        {
            mCalculationTimer.Stop( );
            mCalculationTimer.Start( );
            ShowProgress( );
        }

        void ApplySavedData( )
        {
            try
            {
                textBoxInput.Text = Properties.Settings.Default.LastInput;
                if( !string.IsNullOrWhiteSpace( Properties.Settings.Default.LastMaxDigits ) )
                {
                    comboBoxDigits.Text = Properties.Settings.Default.LastMaxDigits.Trim( );
                }
            }
            catch( Exception exc )
            {
                if( Debugger.IsAttached ) Debugger.Break( );
                else Debug.Fail( exc.Message, exc.ToString( ) );

                // ignore
            }
        }

        void SaveData( )
        {
            Properties.Settings.Default.LastInput = textBoxInput.Text;
            Properties.Settings.Default.LastMaxDigits = comboBoxDigits.Text;

            Properties.Settings.Default.Save( );
        }

        void StopThread( )
        {
            try
            {
                if( mCalculationThread != null )
                {
                    mLastCancellable?.SetCancel( );
                    mCalculationThread.Interrupt( );
                    mCalculationThread.Join( 99 );
                    mCalculationThread = null;
                }
            }
            catch( Exception exc )
            {
                if( Debugger.IsAttached ) Debugger.Break( );
                else Debug.Fail( exc.Message, exc.ToString( ) );

                // ignore?
            }
        }

        void RestartCalculations( )
        {
            try
            {
                StopThread( );

                Fraction? fraction = GetInputFraction( );
                if( fraction == null ) return;

                int? digits = GetDigitsToKeep( );
                if( digits == null ) return;

                mLastCancellable = new SimpleCancellable( );
                mCalculationThread = new Thread( ( ) =>
                    {
                        CalculationThreadProc( mLastCancellable, fraction, digits.Value );
                    } )
                {
                    IsBackground = true,
                    Priority = ThreadPriority.BelowNormal
                };

                mCalculationThread.Start( );
            }
            catch( Exception exc )
            {
                if( Debugger.IsAttached ) Debugger.Break( );

                string error_text = $"Something went wrong.\r\n\r\n{exc.Message}";
                if( Debugger.IsAttached ) error_text = $"{error_text}\r\n{exc.StackTrace}";

                runError.Text = error_text;
                ShowOneRichTextBox( richTextBoxError );
                HideProgress( );
            }
        }

        Fraction? GetInputFraction( )
        {
            string input_text = textBoxInput.Text;

            if( string.IsNullOrWhiteSpace( input_text ) )
            {
                ShowOneRichTextBox( richTextBoxNote );
                HideProgress( );

                return null;
            }

            Match m = RegexToParseInput( ).Match( input_text );

            if( m.Groups["integer"].Success )
            {
                bool is_negative = m.Groups["negative"].Success;
                bool is_negative_exponent = m.Groups["negative_exponent"].Success;
                Group after_point_group = m.Groups["after_point"];
                Group exponent_group = m.Groups["exponent"];

                BigInteger integer = BigInteger.Parse( m.Groups["integer"].Value, CultureInfo.InvariantCulture );
                BigInteger after_point = after_point_group.Success ? BigInteger.Parse( after_point_group.Value, CultureInfo.InvariantCulture ) : BigInteger.Zero;
                BigInteger exponent = exponent_group.Success ? BigInteger.Parse( exponent_group.Value, CultureInfo.InvariantCulture ) : BigInteger.Zero;

                BigInteger significand = integer * BigInteger.Pow( 10, after_point_group.Value.Length ) + after_point; // TODO: trim insignificant zeroes (in Regex)
                BigInteger adjusted_exponent = exponent -= after_point_group.Value.Length;

                var fraction = new Fraction( is_negative ? -significand : significand, BigInteger.One, is_negative_exponent ? -adjusted_exponent : adjusted_exponent );

                return fraction;
            }

            if( m.Groups["nominator"].Success )
            {
                bool is_negative = m.Groups["negative"].Success;
                bool is_negative_exponent = m.Groups["negative_exponent"].Success;
                Group denominator_group = m.Groups["denominator"];
                Group exponent_group = m.Groups["exponent"];

                BigInteger nominator = BigInteger.Parse( m.Groups["nominator"].Value, CultureInfo.InvariantCulture );
                BigInteger denominator = denominator_group.Success ? BigInteger.Parse( denominator_group.Value, CultureInfo.InvariantCulture ) : BigInteger.One;
                BigInteger exponent = exponent_group.Success ? BigInteger.Parse( exponent_group.Value, CultureInfo.InvariantCulture ) : BigInteger.Zero;

                if( denominator.IsZero )
                {
                    runError.Text = "Denominator cannot be zero.";
                    ShowOneRichTextBox( richTextBoxError );
                    HideProgress( );

                    return null;
                }

                var fraction = new Fraction( is_negative ? -nominator : nominator, denominator, is_negative_exponent ? -exponent : exponent );

                return fraction;
            }

            ShowOneRichTextBox( richTextBoxTypicalError );
            HideProgress( );

            return null;
        }

        int? GetDigitsToKeep( )
        {
            string digits_as_string = comboBoxDigits.Text;

            if( !int.TryParse( digits_as_string, out int digits ) || digits <= 0 || digits > MAX_MAXDIGITS )
            {
                runError.Text = $"Please enter a valid number of digits between 1 and {MAX_MAXDIGITS}.";
                ShowOneRichTextBox( richTextBoxError );
                HideProgress( );

                return null;
            }

            return digits;
        }

        void CalculationThreadProc( ICancellable cnc, Fraction fraction, int maxDigits )
        {
            try
            {
                BigInteger max_value = BigInteger.Pow( 10, maxDigits ) - 1;

                Fraction approximated_fraction =
                    fraction
                        .TrimZeroes( cnc )
                        .Reduce( cnc, max_value, noE: true )
                        .TrimZeroes( cnc );

                if( approximated_fraction.Equals( cnc, fraction ) ) approximated_fraction = approximated_fraction.AsNonApprox( );

                ShowResult( cnc, fraction, approximated_fraction, maxDigits );
                HideProgress( );
            }
            catch( OperationCanceledException ) // also 'TaskCanceledException'
            {
                // (the operation is supposed to be restarted)
                return;
            }
            catch( Exception exc )
            {
                if( Debugger.IsAttached ) Debugger.Break( );

                string error_text = $"Something went wrong.\r\n\r\n{exc.Message}";
                if( Debugger.IsAttached ) error_text = $"{error_text}\r\n{exc.StackTrace}";

                runError.Text = error_text;
                ShowOneRichTextBox( richTextBoxError );
                HideProgress( );
            }
        }

        void ShowResult( ICancellable cnc, Fraction initialFraction, Fraction approximatedFraction, int maxDigits )
        {
            BigInteger max_value_exclusive_div_10 = BigInteger.Pow( 10, maxDigits - 1 );

            bool is_negative = approximatedFraction.IsNegative;
            BigInteger n = BigInteger.Abs( approximatedFraction.N );
            BigInteger d = approximatedFraction.D;
            BigInteger e = approximatedFraction.E;

            while( e > 0 && n < max_value_exclusive_div_10 )
            {
                n *= 10;
                --e;
            }

            while( e < 0 && d < max_value_exclusive_div_10 )
            {
                d *= 10;
                ++e;
            }

            string fraction_as_string = $"{( is_negative ? -n : n ):D}";
            if( !e.IsZero ) fraction_as_string = $"{fraction_as_string}e{( e < 0 ? '-' : '+' )}{e:D}";
            if( !d.IsOne ) fraction_as_string = $"{fraction_as_string} / {d:D}";

            string notes = "";

            if( !e.IsZero )
            {
                notes = $"{notes}An exponent is required since the number is not in [-1...+1].\r\n";
            }

            if( approximatedFraction.Equals( cnc, initialFraction ) )
            {
                notes = $"{notes}The initial and approximated fractions are equal.\r\n";
            }

            notes = notes.TrimEnd( );

            string floating_point_form = approximatedFraction.ToFloatString( cnc, 15 );

            CalculationContext ctx = new( cnc, 333 );
            Fraction difference = Fraction.Abs( Fraction.Sub( initialFraction, approximatedFraction, ctx ), ctx );
            string difference_as_string = difference.ToFloatString( cnc, 8 );

            Dispatcher.BeginInvoke( ( ) =>
            {
                runResult.Text = fraction_as_string;
                runResultNotes.Text = notes;
                runResultFloatingPointForm.Text = floating_point_form;
                runResultDifference.Text = difference_as_string;

                ShowOneRichTextBox( richTextBoxResult );
            } );
        }

        void ShowOneRichTextBox( RichTextBox richTextBox )
        {
            richTextBoxNote.Visibility = Visibility.Hidden;
            richTextBoxTypicalError.Visibility = Visibility.Hidden;
            richTextBoxError.Visibility = Visibility.Hidden;
            richTextBoxResult.Visibility = Visibility.Hidden;

            richTextBox.Visibility = Visibility.Visible;
        }

        #region Progress indicator

        void ShowProgress( )
        {
            mProgressTimer.Stop( );
            mProgressStatus = ProgressStatusEnum.None;

            if( mProgressShownTime != DateTime.MinValue )
            {
#if DEBUG
                Dispatcher.Invoke( ( ) =>
                {
                    Debug.Assert( labelPleaseWait.Visibility == Visibility.Visible );
                } );
#endif
                return;
            }
            else
            {
                mProgressStatus = ProgressStatusEnum.DelayToShow;
                mProgressTimer.Interval = DELAY_BEFORE_PROGRESS;
                mProgressTimer.Start( );
            }
        }

        void HideProgress( bool rightNow = false )
        {
            mProgressTimer.Stop( );
            mProgressStatus = ProgressStatusEnum.None;

            if( rightNow || mProgressShownTime == DateTime.MinValue )
            {
                Dispatcher.Invoke( ( ) => labelPleaseWait.Visibility = Visibility.Hidden );
                mProgressShownTime = DateTime.MinValue;
            }
            else
            {
#if DEBUG
                Dispatcher.Invoke( ( ) =>
                {
                    Debug.Assert( labelPleaseWait.Visibility == Visibility.Visible );
                } );
#endif

                TimeSpan elapsed = DateTime.Now - mProgressShownTime;

                if( elapsed >= MIN_DURATION_PROGRESS )
                {
                    Dispatcher.Invoke( ( ) => labelPleaseWait.Visibility = Visibility.Hidden );
                    mProgressShownTime = DateTime.MinValue;
                }
                else
                {
                    mProgressStatus = ProgressStatusEnum.DelayToHide;
                    mProgressTimer.Interval = MIN_DURATION_PROGRESS - elapsed;
                    mProgressTimer.Start( );
                }
            }
        }

        private void ProgressTimer_Tick( object? sender, EventArgs e )
        {
            mProgressTimer.Stop( );

            switch( mProgressStatus )
            {
            case ProgressStatusEnum.DelayToShow:
                labelPleaseWait.Visibility = Visibility.Visible;
                mProgressShownTime = DateTime.Now;
                break;
            case ProgressStatusEnum.DelayToHide:
                labelPleaseWait.Visibility = Visibility.Hidden;
                mProgressShownTime = DateTime.MinValue;
                break;
            default:
                Debug.Assert( false );
                break;
            }

            mProgressStatus = ProgressStatusEnum.None;
        }

        #endregion

        #region Window placement

        void SaveWindowPlacement( )
        {
            try
            {
                Properties.Settings.Default.IsMaximised = WindowState == WindowState.Maximized;
                if( Properties.Settings.Default.IsMaximised )
                {
                    Properties.Settings.Default.RestoreBoundsXY = new System.Drawing.Point( (int)RestoreBounds.Location.X, (int)RestoreBounds.Location.Y );
                    Properties.Settings.Default.RestoreBoundsWH = new System.Drawing.Size( (int)RestoreBounds.Size.Width, (int)RestoreBounds.Size.Height );
                }
                else
                {
                    Properties.Settings.Default.RestoreBoundsXY = new System.Drawing.Point( (int)Left, (int)Top );
                    Properties.Settings.Default.RestoreBoundsWH = new System.Drawing.Size( (int)ActualWidth, (int)ActualHeight );
                }

                Properties.Settings.Default.Save( );
            }
            catch( Exception exc )
            {
                if( Debugger.IsAttached ) Debugger.Break( );
                else Debug.Fail( exc.Message, exc.ToString( ) );

                // ignore
            }
        }

        [StructLayout( LayoutKind.Sequential )]
        struct POINT
        {
            public Int32 X;
            public Int32 Y;
        }

        [DllImport( "user32", SetLastError = true )]
        [DefaultDllImportSearchPathsAttribute( DllImportSearchPath.System32 )]
        static extern IntPtr MonitorFromPoint( POINT pt, Int32 dwFlags );

        const Int32 MONITOR_DEFAULTTONULL = 0;

        static bool IsVisibleOnAnyMonitor( Point px )
        {
            POINT p = new( ) { X = (int)px.X, Y = (int)px.Y };

            return MonitorFromPoint( p, MONITOR_DEFAULTTONULL ) != IntPtr.Zero;
        }

        Point ToPixels( Point p )
        {
            Matrix transform = PresentationSource.FromVisual( this ).CompositionTarget.TransformToDevice;

            Point r = transform.Transform( p );

            return r;
        }

        void RestoreWindowPlacement( )
        {
            try
            {
                Rect r = new( Properties.Settings.Default.RestoreBoundsXY.X, Properties.Settings.Default.RestoreBoundsXY.Y, Properties.Settings.Default.RestoreBoundsWH.Width, Properties.Settings.Default.RestoreBoundsWH.Height );

                if( !r.IsEmpty && r.Width > 0 && r.Height > 0 )
                {
                    // check if the window is in working area
                    // TODO: check if it works with different DPIs

                    Point p1, p2;
                    p1 = r.TopLeft;
                    p1.Offset( 10, 10 );
                    p2 = r.TopRight;
                    p2.Offset( -10, 10 );

                    if( IsVisibleOnAnyMonitor( ToPixels( p1 ) ) || IsVisibleOnAnyMonitor( ToPixels( p2 ) ) )
                    {
                        Left = r.Left;
                        Top = r.Top;
                        Width = Math.Max( 50, r.Width );
                        Height = Math.Max( 40, r.Height );
                    }
                }
                // Note. To work on secondary monitor, the 'Maximized' state is restored in 'Window_SourceInitialized'.
            }
            catch( Exception exc )
            {
                if( Debugger.IsAttached ) Debugger.Break( );
                else Debug.Fail( exc.Message, exc.ToString( ) );

                // ignore
            }
        }

        void RestoreMaximizedState( )
        {
            // restore the Maximized state; this works for secondary monitors as well;
            // to avoid undesirable effects, call it from 'SourceInitialized'
            if( Properties.Settings.Default.IsMaximised )
            {
                WindowState = WindowState.Maximized;
            }
        }

        #endregion


        [GeneratedRegex( """
            (?xn)^ \s* 
            (
             (
              (\+|(?<negative>-))? \s* (?<integer>\d+) 
              ((\s* \. \s* (?<after_point>\d+)) | \.)? 
              (\s* [eE] \s* (\+|(?<negative_exponent>-))? \s* (?<exponent>\d+))? 
             )
            |
             (
              (\+|(?<negative>-))? \s* (?<nominator>\d+) 
              (\s* [eE] \s* (\+|(?<negative_exponent>-))? \s* (?<exponent>\d+))? 
              \s* / \s*
              (?<denominator>\d+) 
             )
            )
            \s* $
            """, RegexOptions.IgnorePatternWhitespace
        )]
        private static partial Regex RegexToParseInput( );

    }
}