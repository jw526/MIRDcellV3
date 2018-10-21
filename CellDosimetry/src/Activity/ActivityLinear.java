package Activity;

import javax.swing.*;
import java.util.Random;

/**
 * Created by ar548 on 7/22/2016.
 */
public class ActivityLinear {
	public static double[][] generateActivity(boolean NECROTIC, boolean DEAD_NECROTIC,
			int cellnumber, int Height, int labelcellnum, int longestaxis, int Radius, double shellWidth,
			String Shape,
			double AccuActivity, double constantProvided, double MeanActivity, double Tau,
			double[][] cell) {
		int TMC = 0, j;
		final double cosT = Math.cos(Math.atan2(Radius, Height));
		double constantUsed, edgeActivity, ratio, rToCell, sum1 = 0.0, toOuterEdge = 0.0;
		Random randomgen = new Random();
		if(NECROTIC){
			
			if(Shape.toLowerCase().equals("sphere")){
				edgeActivity = 1.0 / (Math.PI * shellWidth * shellWidth * shellWidth);
			}
			else if(Shape.toLowerCase().equals("rod")){
				edgeActivity = 1.0;
			}
			else if(Shape.toLowerCase().equals("ellipsoid")){
				toOuterEdge = Math.sqrt(2.0 * Math.pow(Radius / (Radius - shellWidth), 2) + Math.pow(Height / (Height - shellWidth), 2)) - 1.0;
				edgeActivity = 1.0 / (Math.PI * shellWidth * shellWidth * shellWidth);
			}
			else{
				//this is for the cone and is not yet implemented but should never come up anyway
				edgeActivity = 1.0;
			}
			constantUsed = (constantProvided * edgeActivity) / (1 - constantProvided);
			
			// constants needed for ellipsoid
			double innershort = Radius / 2.0 - shellWidth;
			double innerlong = Height / 2.0 - shellWidth;
			
			//constants needed for cone
			final double hypot = Math.sqrt(Radius * Radius + Height * Height);      // the length of the angled edge of the cone
			double rToTop = 0;
			double distFromEdge;
			final double COM = 3.0 * Height / 4.0;
			
			for(int i = 0; i < labelcellnum && AccuActivity > 0; i++){
				j = randomgen.nextInt(cellnumber);
				
				// is the cell labelable?
				if(Shape.toLowerCase().equals("sphere")){
					rToCell = Math.sqrt(Math.pow(cell[j][1], 2) + Math.pow(cell[j][2], 2) + Math.pow(cell[j][3], 2));
					if(cell[j][4] != 0 || rToCell < (longestaxis - shellWidth)){
						i--;
						TMC++;
						if(TMC == cellnumber * 2){
							JOptionPane.showMessageDialog(null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double) i++ / (double) cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE);
							break;
						}
						continue;
					}
				}
				else if(Shape.toLowerCase().equals("rod")){
					rToCell = Math.sqrt(Math.pow(cell[j][1], 2) + Math.pow(cell[j][3], 2));
					rToTop = Math.min(-cell[j][2], Height + cell[j][2]);
					if(cell[j][4] != 0 || (rToCell < (Radius - shellWidth) && rToTop > shellWidth)){
						edgeActivity = Math.max(Math.min(Radius - rToCell, rToTop), edgeActivity);
						i--;
						TMC++;
						if(TMC == cellnumber * 2){
							JOptionPane.showMessageDialog(null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double) i++ / (double) cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE);
							break;
						}
						continue;
					}
				}
				else if(Shape.toLowerCase().equals("ellipsoid")){
					rToCell = Math.sqrt(Math.pow(cell[j][1], 2) / Math.pow(innershort, 2) + Math.pow(cell[j][2], 2) / Math.pow(innerlong, 2) + Math.pow(cell[j][3], 2) / Math.pow(innershort, 2));
					if(cell[j][4] != 0 || rToCell < 1){
						i--;
						TMC++;
						if(TMC == cellnumber * 2){
							JOptionPane.showMessageDialog(null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double) i++ / (double) cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE);
							break;
						}
						continue;
					}
				}
				else if(Shape.toLowerCase().equals("cone")){
					rToCell = Math.sqrt(Math.pow(cell[j][1], 2) + Math.pow(cell[j][3], 2));
					rToTop = Height + cell[j][2];
					distFromEdge = ((double) Radius / (double) Height * Math.abs(cell[j][2]) - rToCell) * cosT;
					
					if(Math.min(rToTop, distFromEdge) > edgeActivity){
						edgeActivity = Math.min(rToTop, distFromEdge);
					}
					if(cell[j][4] != 0 || Math.min(rToTop, distFromEdge) > shellWidth){
						i--;
						TMC++;
						if(TMC == cellnumber * 2){
							JOptionPane.showMessageDialog(null, "You are trying to label more cells than are in the living zone.\nLabeled cells: " + i + "        % of cells labeled: " + ((double) i++ / (double) cellnumber * 100.0) + "\nwhen running future simulation with the same dimensions use these numbers", "Too many cells to label", JOptionPane.WARNING_MESSAGE);
							break;
						}
						continue;
					}
				}
				else{
					JOptionPane.showMessageDialog(null, "Unfortunately Necrotic Geometry is not yet implemented for Conical shapes yet. Sorry for the inconvenience.", "We're Sorry", JOptionPane.WARNING_MESSAGE);
					return cell;
				}
				TMC = 0;
				
				//label the cell
				cell[j][4] = 1; // the cell is labeled
				
				
				// assign the activity based on the distanc from the edge
				if(Shape.toLowerCase().equals("sphere")){
					cell[j][5] = (rToCell - (longestaxis - shellWidth)) / (Math.PI * Math.pow(shellWidth, 4)) + constantUsed;
				}
				else if(Shape.toLowerCase().equals("rod")){
					cell[j][5] = edgeActivity - Math.min(Radius - rToCell, rToTop) + constantUsed * edgeActivity;
				}
				else if(Shape.toLowerCase().equals("ellipsoid")){
					ratio = (rToCell - 1) / toOuterEdge;
					cell[j][5] = ratio * edgeActivity + constantUsed;
				}
				else if(Shape.toLowerCase().equals("cone")){
					rToCell = Math.sqrt(Math.pow(cell[j][1], 2) + Math.pow(cell[j][3], 2));
					rToTop = Height - cell[j][2];
					distFromEdge = ((double) Radius / (double) Height * Math.abs(cell[j][2]) - rToCell) * cosT;
					
					if(rToTop <= distFromEdge){
						cell[j][5] = edgeActivity - rToTop + edgeActivity * constantUsed;
					}
					else{
						cell[j][5] = edgeActivity - distFromEdge + edgeActivity * constantUsed;
					}
				}
				else{
					System.out.println("invalid shape");//somehow...?
					edgeActivity = 0.0;
					//this should never happen
				}
				sum1 += cell[j][5];
			}
			
			for(int i = 0; i < cellnumber; i++){
				System.out.println((int)cell[i][0]);
			}
			
			if(DEAD_NECROTIC){
				for(int i = 0; i < cellnumber; i++){
					if(Shape.toLowerCase().equals("sphere")){
						rToCell = Math.sqrt(Math.pow(cell[i][1], 2) + Math.pow(cell[i][2], 2) + Math.pow(cell[i][3], 2));
						if(rToCell <= (longestaxis - shellWidth)){
							cell[i][0] = 0; // label this cell dead
							continue;
						}
					}
					else if(Shape.toLowerCase().equals("rod")){
						rToCell = Math.sqrt(Math.pow(cell[i][1], 2) + Math.pow(cell[i][3], 2));
						if(rToCell < (Radius - shellWidth)){
							cell[i][0] = 0;
							continue;
						}
					}
					else if(Shape.toLowerCase().equals("ellipsoid")){
						innershort = Radius / 2.0 - shellWidth;
						innerlong = Height / 2.0 - shellWidth;
						rToCell = Math.sqrt(Math.pow(cell[i][1], 2) / Math.pow(innershort, 2) + Math.pow(cell[i][2], 2) / Math.pow(innerlong, 2) + Math.pow(cell[i][3], 2) / Math.pow(innershort, 2));
						if(rToCell < 1){
							cell[i][0] = 0;
							continue;
						}
					}
				}
			}
			
			
		}
		else{
			
			if(Shape.toLowerCase().equals("sphere")){
				edgeActivity = 1.0 / (Math.PI * longestaxis * longestaxis * longestaxis);
				constantUsed = (constantProvided * edgeActivity) / (1 - constantProvided);
			}
			else if(Shape.toLowerCase().equals("rod")){
				edgeActivity = Math.min(Radius, Height);
				constantUsed = (constantProvided * edgeActivity) / (1 - constantProvided);
			}
			else if(Shape.toLowerCase().equals("ellipsoid")){
				edgeActivity = 1;
				constantUsed = constantProvided / (1 - constantProvided);
			}
			else{
				// this is for cone and is not yet used;
				edgeActivity = 0.0;
				constantUsed = 0.0;
			}
			
			for(int i = 0; i < labelcellnum; i++){
				// pick a cell to label if its already labeled try again
				j = randomgen.nextInt(cellnumber);
				if(cell[j][4] != 0){
					i--;
					continue;
				}
				cell[j][4] = 1; // this cell is labeled
				
				if(Shape.toLowerCase().equals("sphere")){
					rToCell = Math.sqrt(Math.pow(cell[j][1], 2) + Math.pow((cell[j][2]), 2) + Math.pow(cell[j][3], 2));
					cell[j][5] = (rToCell / (Math.PI * longestaxis * longestaxis * longestaxis * longestaxis)) + constantUsed;
				}
				else if(Shape.toLowerCase().equals("rod")){
					double rToTop = Math.min(-cell[j][2], Height + cell[j][2]);
					rToCell = Math.sqrt(Math.pow(cell[j][1], 2) + Math.pow(cell[j][3], 2));
					
					cell[j][5] = Math.min(Height - rToTop, Radius - rToCell) + edgeActivity * constantUsed;
					//cell[j][5] = rToCell / (Math.PI * Radius * Radius * Radius) + constantUsed;
				}
				else if(Shape.toLowerCase().equals("ellipsoid")){
					rToCell = Math.sqrt(Math.pow(cell[j][1] / Radius, 2) + Math.pow(cell[j][2] / Height, 2) + Math.pow(cell[j][3] / Radius, 2));
					cell[j][5] = rToCell + constantUsed;
				}
				else if(Shape.toLowerCase().equals("cone")){
					
					double rToTop = Height + cell[j][2];
					rToCell = Math.sqrt(Math.pow(cell[j][1], 2) + Math.pow(cell[j][3], 2));
					double distFromEdge = ((double) Radius / (double) Height * Math.abs(cell[j][2]) - rToCell) * cosT;
					
					if(rToTop <= distFromEdge){
						cell[j][5] = edgeActivity - rToTop + edgeActivity * constantUsed;
					}
					else{
						cell[j][5] = edgeActivity - distFromEdge + edgeActivity * constantUsed;
					}
				}
				sum1 += cell[j][5];
			}
		}
		
		for(int i = 0; i < cellnumber; i++){
			cell[i][5] = cell[i][5] * MeanActivity * labelcellnum / sum1 * Tau;
		}
		
		return cell;
	}
}
