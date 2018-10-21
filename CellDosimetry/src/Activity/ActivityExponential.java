package Activity;

import javax.swing.*;
import java.util.Random;

/**
 * Created by ar548 on 7/22/2016.
 */
public class ActivityExponential {
	public static double[][] generateActivity(boolean NECROTIC, boolean DEAD_NECROTIC,
	                                          int cellnumber, int Height, int labelcellnum, int longestaxis, int Radius, double shellWidth,
	                                          String Shape,
	                                          double AccuActivity, double b, double constantProvided, double MeanActivity, double Tau,
	                                          double[][] cell){
		Random randomgen = new Random();
		double A1, A2, constantUsed, EA1, EA2, edgeActivity, rToCell, sum1 = 0.0, toOuterEdge = 0.0;
		int TMC = 0, j;

		if(NECROTIC){
			if(Shape.toLowerCase().equals( "sphere" )) {
				EA1 = Math.exp( b * (shellWidth) ) - 1.0;
				EA2 = Math.exp( b * shellWidth ) * (shellWidth * shellWidth / b - 2.0 * shellWidth / (b * b) + 2.0 / (b * b * b)) - shellWidth * shellWidth * shellWidth / 3.0 - 2.0 / (b * b * b);
				edgeActivity = EA1 / (EA2 * 4.0 * Math.PI);
			}
			else if(Shape.toLowerCase().equals( "rod" )) {
				// TODO ask dr howell what the exponential equation is for the rod / for 2d shapes
				EA1 = Math.exp( b * (shellWidth) ) - 1.0;
				EA2 = Math.exp( b * (shellWidth) )*(Radius/b - 1.0/(b*b))-Radius*Radius/2.0;
				edgeActivity = EA1 / (EA2 * Math.PI);
			}
			else if(Shape.toLowerCase().equals( "ellipsoid" )) {
				// TODO below may not be right
				toOuterEdge = Math.sqrt( 2.0 * Math.pow( Radius / (Radius - shellWidth), 2 ) + Math.pow( Height / (Height - shellWidth), 2 ) ) - 1.0;
				EA1 = Math.exp( b * (shellWidth) ) - 1.0;
				EA2 = Math.exp( b * shellWidth ) * (shellWidth * shellWidth / b - 2.0 * shellWidth / (b * b) + 2.0 / (b * b * b)) - shellWidth * shellWidth * shellWidth / 3.0 - 2.0 / (b * b * b);
				edgeActivity = EA1 / (EA2 * 4.0 * Math.PI);
			}
			else {
				// this is for cone and is not yet implemented
				edgeActivity = 1.0;
			}
			constantUsed = (constantProvided * edgeActivity) / (1 - constantProvided);

			for(int i = 0; i < labelcellnum && AccuActivity > 0; i++) {
				j = randomgen.nextInt( cellnumber );

				// is the cell labelable?
				if(Shape.toLowerCase().equals( "sphere" )) {
					rToCell = Math.sqrt(Math.pow( cell[j][1], 2 ) + Math.pow( cell[j][2], 2 ) + Math.pow( cell[j][3], 2 ));
					if(cell[j][4] != 0 || rToCell < (longestaxis - shellWidth)) {
						i--;
						TMC++;
						if(TMC == cellnumber * 2) {
							JOptionPane.showMessageDialog( null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double) i / (double) cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE );
							break;
						}
						continue;
					}
				}
				else if(Shape.toLowerCase().equals( "rod" )) {
					rToCell = Math.sqrt(Math.pow( cell[j][1], 2 ) + Math.pow( cell[j][3], 2 ));
					if(cell[j][4] != 0 || rToCell < (Radius - shellWidth) ) {
						i--;
						TMC++;
						if(TMC == cellnumber * 2) {
							JOptionPane.showMessageDialog( null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double) i / (double) cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE );
							break;
						}
						continue;
					}
				}
				else if(Shape.toLowerCase().equals( "ellipsoid" )) {
					double innershort = Radius / 2.0 - shellWidth;
					double innerlong = Height / 2.0 - shellWidth;
					rToCell = Math.sqrt( Math.pow( cell[j][1], 2 ) / Math.pow( innershort, 2 ) + Math.pow( cell[j][2], 2 ) / Math.pow( innerlong, 2 ) + Math.pow( cell[j][3], 2 ) / Math.pow( innershort, 2 ) );
					if(cell[j][4] != 0 || rToCell < 1) {
						i--;
						TMC++;
						if(TMC == cellnumber * 2) {
							JOptionPane.showMessageDialog( null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double) i / (double) cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE );
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

				//label the cell
				cell[j][4] = 1; // the cell is labeled

				if(Shape.toLowerCase().equals( "sphere" )) {
					A1 = Math.pow( Math.E, b * (rToCell - (longestaxis - shellWidth)) ) - 1;
					A2 = Math.pow( Math.E, b * shellWidth ) * (shellWidth * shellWidth / b - 2.0 * shellWidth / (b * b) + 2.0 / (b * b * b)) - shellWidth * shellWidth * shellWidth / 3.0 - 2.0 / (b * b * b);
					cell[j][5] = A1 / (A2 * 4.0 * Math.PI) + constantUsed;
				}
				else if(Shape.toLowerCase().equals( "rod" )) {
					A1 = Math.exp( b * (rToCell - (Radius - shellWidth)) )  - 1.0;
					A2 = Math.exp( b * (shellWidth) )*(shellWidth/b - 1.0/(b*b))-shellWidth*shellWidth/2.0;
					cell[j][5] = A1 / (A2 * 2.0 * Math.PI) + constantUsed;
				}
				else if(Shape.toLowerCase().equals( "ellipsoid" )) {
					double ratio = (rToCell - 1) / toOuterEdge;
					//TODO the below may not work
					A1 = Math.exp( b * (ratio)) - 1;
					A2 = Math.pow( Math.E, b * 1.0 ) * (1.0/b - 2.0 * 1.0 / (b * b) + 2.0 / (b * b * b)) - 1.0 / 3.0 - 2.0 / (b * b * b);
					cell[j][5] = A1 / (A2 * 4.0 * Math.PI) + constantUsed;
				}
				else {
					//this is for the cone and is not yet implemented but should never come up anyway
					cell[j][5] = 0.0 + constantUsed;
					edgeActivity = 0.0;
				}
				sum1 += cell[j][5];
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
			if(Shape.toLowerCase().equals( "sphere" )) {
				edgeActivity = 1.0 / (Math.PI * longestaxis * longestaxis * longestaxis);
				constantUsed = (constantProvided * edgeActivity) / (1 - constantProvided);
			}
			else if(Shape.toLowerCase().equals( "rod" )) {
				edgeActivity = 1.0 / (Math.PI * Radius * Radius);
				constantUsed = (constantProvided * edgeActivity) / (1 - constantProvided);
			}
			else if(Shape.toLowerCase().equals( "ellipsoid" )) {
				edgeActivity = 1;
				constantUsed = constantProvided / (1 - constantProvided);
			}
			else {
				// this is for cone and is not yet used;
				edgeActivity = 0.0;
				constantUsed = 0.0;
			}

			for(int i = 0; i < labelcellnum; i++) {
				// pick a cell to label.  if its already labeled try again
				j = randomgen.nextInt( cellnumber );
				if(cell[j][4] != 0) {
					i--;
					continue;
				}
				cell[j][4] = 1; // this cell is labeled

				if(Shape.toLowerCase().equals( "sphere" )){
					rToCell = Math.sqrt( Math.pow( cell[j][1], 2 ) + Math.pow( (cell[j][2]), 2 ) + Math.pow( cell[j][3], 2 ) );
					A1 = Math.exp(b*rToCell) - 1.0 ;
					A2 = Math.pow( Math.E, b * longestaxis ) * (longestaxis * longestaxis / b - 2.0 * longestaxis / (b * b) + 2.0 / (b * b * b)) - longestaxis * longestaxis * longestaxis / 3.0 - 2.0 / (b * b * b);
					cell[j][5] = A1 / (Math.PI * 4.0 * A2) + constantUsed;
				}
				else if( Shape.toLowerCase().equals( "rod" ) ){
					rToCell = Math.sqrt( Math.pow( cell[j][1], 2 ) + Math.pow( cell[j][3], 2 ) );
					A1 = Math.exp( b * rToCell) - 1.0;
					A2 = Math.exp( b * (longestaxis) )*(longestaxis/b - 1.0/(b*b))-longestaxis*longestaxis/2.0;
					cell[j][5] = A1 / (A2 * 2.0 * Math.PI) + constantUsed;
				}
				else if (Shape.toLowerCase().equals( "ellipsoid" )){
					rToCell = Math.sqrt( Math.pow( cell[j][1] / Radius, 2 ) + Math.pow( cell[j][2] / Height, 2 ) + Math.pow( cell[j][3] / Radius, 2 ) );
					A1 = Math.exp(b*rToCell) - 1.0 ;
					A2 = Math.pow( Math.E, b * 1.0 ) * (1.0 / b - 2.0 / (b * b) + 2.0 / (b * b * b)) - 1.0 / 3.0 - 2.0 / (b * b * b);
					cell[j][5] = A1 / (Math.PI * 4.0 * A2) + constantUsed;
				}else{
					// this is for the cone and is not yet implemented yet
					cell[j][5] = 0.0 + constantUsed;
				}
				sum1 += cell[j][5];
			}
		}

		for(int i = 0; i < cellnumber; i++) {
			if(cell[i][4] != 0){
				cell[i][5] = cell[i][5] * MeanActivity * labelcellnum / sum1 * Tau;
			}
		}

		return cell;
	}
}
