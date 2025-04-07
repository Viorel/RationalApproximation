﻿using System.Diagnostics;
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
        const int ACCEPTABLE_PERCENT_ERROR = 10;
        readonly TimeSpan DELAY_BEFORE_CALCULATION = TimeSpan.FromMilliseconds( 333 );
        readonly TimeSpan DELAY_BEFORE_PROGRESS = TimeSpan.FromMilliseconds( 333 );
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

            // TODO: trim insignificant zeroes (in Regex)

            Match m = RegexToParseInput( ).Match( input_text );

            if( m.Groups["integer"].Success )
            {
                bool is_negative = m.Groups["negative"].Success;
                bool is_exponent_negative = m.Groups["negative_exponent"].Success;
                Group floating_group = m.Groups["floating"];
                Group repeating_group = m.Groups["repeating"];
                Group exponent_group = m.Groups["exponent"];

                BigInteger integer = BigInteger.Parse( m.Groups["integer"].Value, CultureInfo.InvariantCulture );
                BigInteger exponent = exponent_group.Success ? BigInteger.Parse( exponent_group.Value, CultureInfo.InvariantCulture ) : BigInteger.Zero;
                if( is_exponent_negative ) exponent = -exponent;

                if( floating_group.Success || repeating_group.Success )
                {
                    // 123.45, 123.45(67), 123.(67), maybe with e

                    BigInteger floating = floating_group.Success ? BigInteger.Parse( floating_group.Value, CultureInfo.InvariantCulture ) : BigInteger.Zero;
                    int floating_length = floating_group.Success ? floating_group.Value.Length : 0;
                    BigInteger floating_magnitude = BigInteger.Pow( 10, floating_length );

                    if( repeating_group.Success )
                    {
                        // 123.45(67), 123.(67), maybe with e

                        BigInteger repeating = BigInteger.Parse( repeating_group.Value, CultureInfo.InvariantCulture );
                        int repeating_length = repeating_group.Value.Length;

                        BigInteger repeating_magnitude = BigInteger.Pow( 10, repeating_length );

                        BigInteger significant = integer * floating_magnitude + floating;
                        BigInteger significant_with_repeating = significant * repeating_magnitude + repeating;
                        Debug.Assert( significant_with_repeating >= significant );
                        BigInteger nominator = significant_with_repeating - significant;
                        BigInteger denominator = floating_magnitude * ( repeating_magnitude - 1 );

                        Fraction fraction = new( is_negative ? -nominator : nominator, denominator, exponent );

                        return fraction;
                    }
                    else
                    {
                        // 123.45, maybe with e

                        BigInteger significant = integer * floating_magnitude + floating;
                        BigInteger adjusted_exponent = exponent - floating_length;

                        Fraction fraction = new( is_negative ? -significant : significant, BigInteger.One, adjusted_exponent );

                        return fraction;
                    }
                }
                else
                {
                    // 123, 123e45

                    Fraction fraction = new( is_negative ? -integer : integer, BigInteger.One, exponent );

                    return fraction;
                }
            }

            if( m.Groups["nominator"].Success )
            {
                bool is_negative = m.Groups["negative"].Success;
                bool is_exponent_negative = m.Groups["negative_exponent"].Success;
                Group denominator_group = m.Groups["denominator"];
                Group exponent_group = m.Groups["exponent"];

                BigInteger nominator = BigInteger.Parse( m.Groups["nominator"].Value, CultureInfo.InvariantCulture );
                BigInteger denominator = denominator_group.Success ? BigInteger.Parse( denominator_group.Value, CultureInfo.InvariantCulture ) : BigInteger.One;
                BigInteger exponent = exponent_group.Success ? BigInteger.Parse( exponent_group.Value, CultureInfo.InvariantCulture ) : BigInteger.Zero;
                if( is_exponent_negative ) exponent = -exponent;

                if( denominator.IsZero )
                {
                    runError.Text = "Denominator cannot be zero.";
                    ShowOneRichTextBox( richTextBoxError );
                    HideProgress( );

                    return null;
                }

                var fraction = new Fraction( is_negative ? -nominator : nominator, denominator, exponent );

                return fraction;
            }

            if( m.Groups["pi"].Success )
            {
                return Fraction.Pi;
            }

            if( m.Groups["e"].Success )
            {
                return Fraction.EulerNumber;
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
                BigInteger max_val = BigInteger.Pow( 10, maxDigits ) - 1;
                CalculationContext ctx = new( cnc, 33 );

                Fraction approximated_fraction =
                    fraction
                        .Simplify( ctx )
                        .TrimZeroes( cnc )
                        .ReduceNoE( cnc, max_val )
                        .TrimZeroes( cnc );

                Fraction? alternative = null;

                if( approximated_fraction.Equals( cnc, fraction ) )
                {
                    approximated_fraction = approximated_fraction.AsNonApprox( );
                }
                else
                {
                    Fraction absolute_error = Fraction.Sub( approximated_fraction, fraction, ctx );
                    Fraction percent_error = Fraction.Mul( Fraction.Div( absolute_error, fraction, ctx ), new Fraction( 100 ), ctx );
                    Fraction percent_error_abs = Fraction.Abs( percent_error, ctx );

                    if( percent_error_abs.CompareTo( cnc, new Fraction( ACCEPTABLE_PERCENT_ERROR ) ) > 0 )
                    {
                        alternative =
                            fraction
                                .Simplify( ctx )
                                .TrimZeroes( cnc )
                                .Reduce( cnc, max_val )
                                .TrimZeroes( cnc );
                    }
                }

                ShowResult( cnc, fraction, approximated_fraction, alternative, maxDigits );
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

                Dispatcher.BeginInvoke( ( ) =>
                {
                    runError.Text = error_text;
                    ShowOneRichTextBox( richTextBoxError );
                    HideProgress( );
                } );
            }
        }

        void ShowResult( ICancellable cnc, Fraction initialFraction, Fraction approximatedFraction, Fraction? alternative, int maxDigits )
        {
            var strings = MakeStrings( cnc, initialFraction, approximatedFraction, maxDigits );

            strings.remarks = strings.remarks.TrimEnd( );

            Dispatcher.BeginInvoke( ( ) =>
            {
                runResultRationalApproximation.Text = strings.fraction_as_string;
                runResultRationalApproximationNote.Text = strings.note;
                runResultFloatingPointForm.Text = strings.floating_point_form;
                runResultAbsoluteError.Text = strings.absolute_error_as_string;
                runResultPercentError.Text = strings.percent_error_as_string;
                runResultRemarks.Text = strings.remarks;

                ShowOneRichTextBox( richTextBoxResult );
            } );

            if( alternative == null )
            {
                Dispatcher.BeginInvoke( ( ) =>
                {
                    if( sectionAlternativeParent.Blocks.Contains( sectionAlternative ) ) sectionAlternativeParent.Blocks.Remove( sectionAlternative );
                } );
            }
            else
            {
                var strings_alternative = MakeStrings( cnc, initialFraction, alternative, maxDigits );
                strings_alternative.remarks = strings_alternative.remarks.TrimEnd( );

                Dispatcher.BeginInvoke( ( ) =>
                {
                    if( !sectionAlternativeParent.Blocks.Contains( sectionAlternative ) ) sectionAlternativeParent.Blocks.Add( sectionAlternative );

                    runResultRationalApproximation2.Text = strings_alternative.fraction_as_string;
                    runResultRationalApproximationNote2.Text = strings_alternative.note;
                    runResultFloatingPointForm2.Text = strings_alternative.floating_point_form;
                    runResultAbsoluteError2.Text = strings_alternative.absolute_error_as_string;
                    runResultPercentError2.Text = strings_alternative.percent_error_as_string;
                    runResultRemarks2.Text = strings_alternative.remarks;
                } );
            }

            static
                (
                    string fraction_as_string,
                    string note,
                    string floating_point_form,
                    string absolute_error_as_string,
                    string percent_error_as_string,
                    string remarks
                )
                MakeStrings( ICancellable cnc, Fraction initialFraction, Fraction approximatedFraction, int maxDigits )
            {
                string fraction_as_string;
                string note = "";
                string floating_point_form;
                string absolute_error_as_string;
                string percent_error_as_string;
                string remarks = "";

                if( approximatedFraction.Equals( cnc, initialFraction ) )
                {
                    remarks = $"{remarks}The given value and its approximation are equal.\r\n";
                    note = "(exact)";
                }

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

                fraction_as_string = $"{( is_negative ? -n : n ):D}";
                if( !e.IsZero ) fraction_as_string = $"{fraction_as_string}e{( e >= 0 ? "+" : "" )}{e:D}";
                if( !d.IsOne ) fraction_as_string = $"{fraction_as_string} / {d:D}";

                floating_point_form = approximatedFraction.ToFloatString( cnc, 15 );

                CalculationContext ctx = new( cnc, 33 );
                Fraction absolute_error = Fraction.Sub( approximatedFraction, initialFraction, ctx );
                absolute_error_as_string = absolute_error.ToFloatString( cnc, 8 );

                if( initialFraction.IsZero )
                {
                    percent_error_as_string = "—";
                }
                else
                {
                    Fraction percent_error = Fraction.Mul( Fraction.Div( absolute_error, initialFraction, ctx ), new Fraction( 100 ), ctx );
                    Fraction percent_error_abs = Fraction.Abs( percent_error, ctx );

                    percent_error_as_string = $"{( percent_error.IsApprox ? "≈" : "" )}{percent_error.ToDouble( ):g4}%";

                    if( percent_error_abs.CompareTo( cnc, new Fraction( ACCEPTABLE_PERCENT_ERROR ) ) > 0 )
                    {
                        remarks = $"{remarks}The error is too large. Not enough digits.\r\n";
                        note = approximatedFraction.IsZero ? "(underflow)" : "(overflow)";
                    }
                }

                return
                    (
                        fraction_as_string,
                        note,
                        floating_point_form,
                        absolute_error_as_string,
                        percent_error_as_string,
                        remarks
                    );
            }
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
            (?xni)^ \s* 
            (
             (
              (\+|(?<negative>-))? \s* (?<integer>\d+) 
              ((\s* \. \s* (?<floating>\d+)) | \.)? 
              (\s* \( \s* (?<repeating>\d+) \s* \) )? 
              (\s* [eE] \s* (\+|(?<negative_exponent>-))? \s* (?<exponent>\d+))? 
             )
            |
             (
              (\+|(?<negative>-))? \s* (?<nominator>\d+) 
              (\s* [eE] \s* (\+|(?<negative_exponent>-))? \s* (?<exponent>\d+))? 
              \s* / \s*
              (?<denominator>\d+) 
             )
            |
             (?<pi>pi | π)
            |
             (?<e>e)
                        )
            \s* $
            """, RegexOptions.IgnorePatternWhitespace
        )]
        private static partial Regex RegexToParseInput( );

    }
}