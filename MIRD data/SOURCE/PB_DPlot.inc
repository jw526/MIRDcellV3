' PowerBASIC-specific DPlot include file: PB_DPlot.inc
%DPLOT_DDE_VERSION = 2

TYPE DPLOT
    Version AS LONG                 ' version number of this structure
    hwnd AS LONG                    ' handle of client application window
    DataFormat AS LONG              ' XY pairs, DX and Y, etc. See DATA_ constants
    MaxCurves AS LONG               ' DataFormat    Description
                                    ' DATA_XYXY     Max. number of curves, <= 20
                                    ' DATA_DXY      ..
                                    ' DATA_XYYY     ..
                                    ' DATA_3D       Number of grid cells in X direction,
                                    '                 = number of data columns-1
                                    ' DATA_3DR      not used
                                    ' DATA_1D       Max. number of groups, <= 20
    MaxPoints AS LONG               ' DATA_XYXY     Max. number of points/curve
                                    ' DATA_DXY      ..
                                    ' DATA_XYYY     ..
                                    ' DATA_3D       Number of grid cells in Y direction,
                                    '                 = number of data rows-1
                                    ' DATA_3DR      Number of X,Y,Z triplets
                                    ' DATA_1D       Max. number of points/group
    NumCurves AS LONG               ' Actual number of curves/groups, always 1 for
                                    '   DATA_3D or DATA_3DR
    ScaleCode AS LONG               ' scaling code for XY plots (Linear, Log, etc.)
    LegendX AS SINGLE               ' left coord of legend, expressed as a ratio
                                    '    of plot size (0->1) (CAN be negative and/or > 1,
                                    '    but exact placement is then a bit hard to predict)
    LegendY AS SINGLE               ' top coord of legend, again expressed as a ratio
                                    '    of plot size
    NP(0 TO 19) AS LONG             ' actual number of points in each curve/group for
                                    '   XY/1D plots; cannot exceed MaxPoints.
    LineType(0 TO 19) AS LONG       ' line types (see codes below)
    SymbolType(0 TO 19) AS LONG     ' symbol types (see codes below)
    SizeofExtraInfo AS LONG         ' Extra information following X,Y data. No need to
                                    '   fill in this member for DPLOTLIB calls
    Legend(0 TO 20) AS STRING * 40  ' 0 is caption for legends box and 1 ..., is
                                    ' the caption for each curve

    Label(0 TO 19) AS STRING * 5    ' Strings displayed beside the last data point
                                    '   in a curve, 5 characters per curve
    Title(0 TO 2) AS STRING * 80
    XAxis AS STRING * 80            ' X Axis label.
    YAxis AS STRING * 80            ' Y Axis label.
END TYPE

TYPE DPLOT_PLOTMETRICS
    size AS LONG		' Size of this structure; should be set by the caller
    hll AS LONG			' horizontal and
    vll AS LONG			'   vertical coordinates of the lower left corner of
				'   the plot, in pixels
    hur AS LONG			' horizontal and
    vur AS LONG			'   vertical coordinates of the upper right corner of
				'   the plot, in pixels
    xlo AS SINGLE		' value of x at the left plot extent
    ylo AS SINGLE		' value of y at the bottom plot extent
    xhi AS SINGLE		' value of x at the right plot extent
    yhi AS SINGLE		' value of z at the top plot extent
END TYPE

DECLARE FUNCTION DPlot_AddData LIB "dplotlib.dll" ALIAS "DPlot_AddData" (BYVAL DocNum AS LONG, BYVAL DataType AS LONG, BYVAL NumPts AS LONG, BYVAL Curve AS LONG, BYREF x AS ANY, BYREF y AS ANY) AS LONG
DECLARE FUNCTION DPlot_Command LIB "dplotlib.dll" ALIAS "DPlot_Command" (BYVAL DocNum AS LONG, BYVAL Command AS STRING) AS LONG
DECLARE FUNCTION DPlot_GetBitmap LIB "dplotlib.dll" ALIAS "DPlot_GetBitmap" (BYVAL DocNum AS LONG, BYVAL cx AS LONG, BYVAL cy AS LONG) AS LONG
DECLARE FUNCTION DPlot_GetBitmapEx LIB "dplotlib.dll" ALIAS "DPlot_GetBitmapEx" (BYVAL DocNum AS LONG, BYVAL cx AS LONG, BYVAL cy AS LONG, BYREF DPM AS DPLOT_PLOTMETRICS) AS LONG
DECLARE FUNCTION DPlot_GetEnhMetaFile LIB "dplotlib.dll" ALIAS "DPlot_GetEnhMetaFile" (BYVAL DocNum AS LONG,BYVAL cx AS SINGLE, BYVAL cy AS SINGLE) AS LONG
DECLARE FUNCTION DPlot_Plot LIB "dplotlib.dll" ALIAS "DPlot_Plot" (BYREF D AS DPLOT, BYREF x AS ANY, BYREF y AS ANY, BYVAL cmds AS STRING) AS LONG
DECLARE FUNCTION DPlot_PlotBitmap LIB "dplotlib.dll" ALIAS "DPlot_PlotBitmap" (BYREF D AS DPLOT, BYREF x AS ANY, BYREF y AS ANY, BYVAL cmds AS STRING, BYVAL cx AS LONG, BYVAL cy AS LONG) AS LONG
DECLARE FUNCTION DPlot_PlotToRect LIB "dplotlib.dll" ALIAS "DPlot_PlotToRect" (BYREF D AS DPLOT, BYREF x AS ANY, BYREF y AS ANY, BYVAL cmds AS STRING, BYVAL hwnd AS LONG, BYREF rc AS RECT) AS LONG
DECLARE FUNCTION DPlot_Request LIB "dplotlib.dll" ALIAS "DPlot_Request" (BYVAL DocNum AS LONG, BYVAL Item AS STRING, BYVAL Value AS STRING, BYREF MaxCount AS LONG) AS LONG
DECLARE SUB DPlot_SetErrorMethod LIB "dplotlib.dll" ALIAS "DPlot_SetErrorMethod" (BYVAL Method AS LONG)
DECLARE FUNCTION DPlot_Start LIB "dplotlib.dll" ALIAS "DPlot_Start" (BYVAL Minimize AS LONG, BYREF WasActive AS LONG) AS LONG
DECLARE SUB DPlot_Stop LIB "dplotlib.dll" ALIAS "DPlot_Stop" ()

' XY Scaling codes:

%SCALE_LINEARX_LINEARY = 1
%SCALE_LINEARX_LOGY = 2
%SCALE_LOGX_LINEARY = 3
%SCALE_LOGX_LOGY = 4
%SCALE_TRIPARTITE = 5
%SCALE_LINEARX_PROBABILITY = 6
%SCALE_GRAINSIZE_DIST = 7
%SCALE_POLAR = 8
%SCALE_BARCHART = 9
%SCALE_LOGX_PROBABILITY = 10
%SCALE_PROBX_LINEARY = 11
%SCALE_PROBX_LOGY = 12
%SCALE_PROBX_PROBY = 13

' Unit-specific scaling codes ... combine with above values using OR operator

%UNITS_DEFAULT = 0
%UNITS_TRIPARTITE_INCHES = %UNITS_DEFAULT           ' Velocity (Y) in inches/sec
%UNITS_TRIPARTITE_FEET = &H100                      '                 feet/sec
%UNITS_TRIPARTITE_MILLIMETERS = &H200               '                 mm/sec
%UNITS_TRIPARTITE_CENTIMETERS = &H300               '                 cm/sec
%UNITS_TRIPARTITE_METERS = &H400                    '                 meters/sec
%UNITS_GRAINSIZE_MILLIMETERS = %UNITS_DEFAULT       ' Grain sizes (X) in mm
%UNITS_GRAINSIZE_INCHES = &H100                     '                 inches
%UNITS_POLAR_RADIANS = %UNITS_DEFAULT               ' Rotation (X) in radians
%UNITS_POLAR_DEGREES = &H100                        '                 degrees
%UNITS_USERDEFINED = &H7F00

' Data organization following DPLOT structure:

%DATA_XYXY = 0          ' One or more sets of X,Y data
%DATA_DXY = 1           ' One or more X,Y curves. Constant spacing in X and same number of points in all curves.
%DATA_XYYY = 2          ' One or more X,Y curves. All curves have the same X values.
%DATA_3D = 3            ' Z values on a rectangular grid
%DATA_3DR = 4           ' Random X,Y,Z values
%DATA_IMAGE = 5         ' Used only by DPlot - there's no way to SEND DPlot an image
%DATA_1D = 6            ' One or more groups of Y values.

' Line styles:
%LINESTYLE_NONE = 0
%LINESTYLE_SOLID = 1
%LINESTYLE_LONGDASH = 2
%LINESTYLE_DOTTED = 3
%LINESTYLE_DASHDOT = 4
%LINESTYLE_DOTDASH = 4
%LINESTYLE_MEDDASH = 5
%LINESTYLE_DASHDOTDOT = 6

' Symbol styles
%SYMBOLSTYLE_NONE = 0
%SYMBOLSTYLE_DOT = 1
%SYMBOLSTYLE_CROSS = 2
%SYMBOLSTYLE_ASTERISK = 3
%SYMBOLSTYLE_X = 4
%SYMBOLSTYLE_SQUARE = 5
%SYMBOLSTYLE_DIAMOND = 6
%SYMBOLSTYLE_TRIANGLE = 7
%SYMBOLSTYLE_OCTAGON = 8
%SYMBOLSTYLE_ITRIANGLE = 9
%SYMBOLSTYLE_HEXAGON = 10
%SYMBOLSTYLE_PENTAGON = 11
%SYMBOLSTYLE_STAR = 12
%SYMBOLSTYLE_FILL = &H100       ' May be combined with other styles
