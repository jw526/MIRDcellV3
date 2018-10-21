package Activity;

import javax.swing.*;
import java.util.Random;

/**
 * Created by Alex Rosen on 7/22/2016.
 */

public class ActivityNormal {
	public static double[][] generateActivity(boolean NECROTIC, boolean DEAD_NECROTIC,
	                                          int cellnumber, int Height, int labelcellnum, int longestaxis, int Radius, double shellWidth,
	                                          String Shape,
	                                          double AccuActivity, double MeanActivity, double ShapeFactor, double Tau,
	                                          double[][] cell, double[] NormalD){
		int TMC = 0, j;
		Random randomgen = new Random();
		double rToTop = 0, distFromEdge;
		final double cosT = Math.cos( Math.atan2( Radius, Height ) );
		final double COM = 3.0 * Height / 4.0;
		if(NECROTIC){
			double rToCell;
			for(int i = 0; i < labelcellnum && AccuActivity > 0; i++) {
				j = randomgen.nextInt( cellnumber );

				// is the cell labelable?
				if(Shape.toLowerCase().equals( "sphere" )) {
					rToCell = Math.pow( cell[j][1], 2 ) + Math.pow( cell[j][2], 2 ) + Math.pow( cell[j][3], 2 );
					if(cell[j][4] != 0 || rToCell < (longestaxis - shellWidth) * (longestaxis - shellWidth)) {
						i--;
						TMC++;
						if(TMC == cellnumber * 2) {
							JOptionPane.showMessageDialog( null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double)i / (double)cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE );
							break;
						}
						continue;
					}
				}
				else if(Shape.toLowerCase().equals( "rod" )) {
					rToCell = Math.pow( cell[j][1], 2 ) + Math.pow( cell[j][3], 2 );
					if(cell[j][4] != 0 || rToCell < (Radius - shellWidth) * (Radius - shellWidth)) {
						i--;
						TMC++;
						if(TMC == cellnumber * 2) {
							JOptionPane.showMessageDialog( null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double)i / (double)cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE );
							break;
						}
						continue;
					}
				}
				else if(Shape.toLowerCase().equals( "ellipsoid" )) {
					double innershort = Radius/2.0-shellWidth;
					double innerlong = Height/2.0-shellWidth;
					rToCell = Math.sqrt(Math.pow( cell[j][1], 2 ) / Math.pow( innershort, 2 ) + Math.pow( cell[j][2], 2 ) / Math.pow( innerlong, 2 ) + Math.pow( cell[j][3], 2 ) / Math.pow( innershort, 2 ));
					if(cell[j][4] != 0 || rToCell < 1 ) {
						i--;
						TMC++;
						if(TMC == cellnumber * 2) {
							JOptionPane.showMessageDialog( null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double)i / (double)cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE );
							break;
						}
						continue;
					}
				}
				else if(Shape.toLowerCase().equals( "cone" )) {
					rToCell = Math.sqrt( Math.pow( cell[j][1], 2 ) + Math.pow( cell[j][3], 2 ) );
					rToTop = Height + cell[j][2];
					distFromEdge = ((double) Radius / (double) Height * Math.abs( cell[j][2] ) - rToCell) * cosT;

					if(cell[j][4] != 0 || Math.min( rToTop, distFromEdge ) > shellWidth) {
						i--;
						TMC++;
						if(TMC == cellnumber * 2) {
							JOptionPane.showMessageDialog( null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double) i++ / (double) cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE );
							break;
						}
						continue;
					}
				}
				else {
					JOptionPane.showMessageDialog( null, "Unfortunately Necrotic Geometry is not yet implemented for Conical shapes yet. Sorry for the inconvenience.", "We're Sorry", JOptionPane.WARNING_MESSAGE );
					return cell;
				}
				TMC = 0;

				cell[j][4] = 1; // the cell is labeled
				cell[j][5] = (MeanActivity + NormalD[i]) * Tau; // this is the activity assigned to the cell
			}
                        
                        if(DEAD_NECROTIC){
                            for(int i = 0; i < cellnumber; i++){
                                if(Shape.toLowerCase().equals( "sphere" )) {
					rToCell = Math.sqrt(Math.pow( cell[i][1], 2 ) + Math.pow( cell[i][2], 2 ) + Math.pow( cell[i][3], 2 ));
					if(rToCell <= (longestaxis - shellWidth)) {
						cell[i][0] = 0; // label this cell dead
						continue;
					}
				}
				else if(Shape.toLowerCase().equals( "rod" )) {
					rToCell = Math.sqrt(Math.pow( cell[i][1], 2 ) + Math.pow( cell[i][3], 2 ));
					if(rToCell < (Radius - shellWidth) ) {
                                            cell[i][0] = 0;
                                            continue;
					}
				}
				else if(Shape.toLowerCase().equals( "ellipsoid" )) {
					double innershort = Radius / 2.0 - shellWidth;
					double innerlong = Height / 2.0 - shellWidth;
					rToCell = Math.sqrt( Math.pow( cell[i][1], 2 ) / Math.pow( innershort, 2 ) + Math.pow( cell[i][2], 2 ) / Math.pow( innerlong, 2 ) + Math.pow( cell[i][3], 2 ) / Math.pow( innershort, 2 ) );
					if(rToCell < 1) {
                                                cell[i][0] = 0;
						continue;
					}
				} 
                            }
                        }
                        
		}
		else{

			for(int i = 0; i < labelcellnum && AccuActivity > 0; i++) {
				j = randomgen.nextInt( cellnumber );
				if(cell[j][4] != 0) {
					i--;
				}
				else {
					cell[j][4] = 1;
					cell[j][5] = (MeanActivity + NormalD[i]) * Tau;
				}
			}

		}
		return cell;
	}
}
