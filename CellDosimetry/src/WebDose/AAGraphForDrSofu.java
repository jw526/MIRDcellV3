package WebDose;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLine3DRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import javax.swing.*;
import java.awt.*;

/**
 * Created by Alex Rosen on 8/22/2016.
 */
public class AAGraphForDrSofu {
	private JPanel chartPanel;
	private double[][] dose;

	public AAGraphForDrSofu(double[][] d){
		this.dose = d;

		String xlabel = "Radial Distance (μm)";
		String ylabel = "Average Dose (Bq)";
		XYSeries xys = new XYSeries( "Dose vs Distance" );
		for(int i = 0; i < dose[0].length; i++) {
			xys.add(dose[0][i], dose[1][i]);
		}
		XYSeriesCollection doseData = new XYSeriesCollection();
		doseData.addSeries( xys );

		JFreeChart doseChart = ChartFactory.createXYLineChart(
				"Dose vs Radial Distance",
				xlabel,
				ylabel,
				(XYDataset)xys,
				PlotOrientation.VERTICAL, // Plot Orientation
				false, // Show Legend
				true, // Use tooltips
				false // Configure chart to generate URLs?
		);

		XYPlot plot = doseChart.getXYPlot();

		NumberAxis domainAxis = new NumberAxis( xlabel );
		domainAxis.setStandardTickUnits( NumberAxis.createIntegerTickUnits() );
		NumberAxis rangeAxis = new NumberAxis( ylabel );
		rangeAxis.setStandardTickUnits( NumberAxis.createStandardTickUnits() );

		plot.setForegroundAlpha( 0.7F );
		plot.setBackgroundPaint( Color.WHITE );
		plot.setDomainGridlinePaint( new Color( 150, 150, 150 ) );
		plot.setRangeGridlinePaint( new Color( 150, 150, 150 ) );

		XYLineAndShapeRenderer xylsr = (XYLineAndShapeRenderer) plot.getRenderer();
		ChartPanel c = new ChartPanel( doseChart );
		chartPanel.removeAll();
		chartPanel.setLayout( new java.awt.BorderLayout() );
		chartPanel.add( c, BorderLayout.CENTER );
		chartPanel.validate();

	}
}
