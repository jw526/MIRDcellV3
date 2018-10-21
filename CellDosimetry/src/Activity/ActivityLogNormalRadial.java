package Activity;

import javax.swing.JOptionPane;
import java.util.Random;

/**
 * Created by Alex Rosen on 11/12/2016.
 */
public class ActivityLogNormalRadial {
	public static double[][] generateActivity(
			boolean NECROTIC, 
			double a, double b, double x0, double y0,
	        int cellnumber, int Radius, int labelcellnum,
	        String Shape,
	        double AccuActivity, double constantProvided, double MeanActivity, double Tau,
	        double[][] cell
	){

		int TMC = 0, j;
		Random randomgen = new Random();
		double constantUsed, edgeActivity, ratio, rToCell, rho, sum1 = 0.0, toOuterEdge = 0.0;

		if(NECROTIC){
			JOptionPane.showMessageDialog( null,  "necrotic not implemented here yet", "Sorry for the inconvenience", JOptionPane.WARNING_MESSAGE);
			return null;
		}
		else{
			if(!Shape.toLowerCase().equals( "sphere" )){
				JOptionPane.showMessageDialog( null,  "Only sphereical is implemented so far", "Sorry for the inconvenience", JOptionPane.WARNING_MESSAGE);
				return null;
			}

			for(int i = 0; i < labelcellnum; i++){
				j = randomgen.nextInt( cellnumber );
				if(cell[j][4] != 0){
					i--;
					continue;
				}

				cell[j][4] = 1; // this cell si labeled

				rToCell = Math.sqrt( Math.pow( cell[j][1], 2 ) + Math.pow( (cell[j][2]), 2 ) + Math.pow( cell[j][3], 2 ) );
				// y = y0 + (a/x) * Math.exp((-1/2)*Math.pow((Math.log(x/x0)/b), 2));
				/**
				 * assuming
				 * y0 = initial activity
				 * y = activity
				 * x = radial position
				 * x0 = edge of hte cell (max radial position
				 * a = ????
				 * b = ????
				 */
				//double x = Radius - rToCell;
				double x = rToCell;
				rho = y0 + (a/x) * Math.exp((-1.0/2.0)*Math.pow((Math.log(x/x0)/b), 2));
				if(rho < 0){
					rho = 0;
					System.err.println( "too low!!!" );
				}
				else if (Double.isNaN( rho )){
					System.err.println( "whoops!?!?!?" );
					rho = 0.0;
				}
				cell[j][5] = rho;
				sum1 += rho;
			}
		}
		for(int i = 0; i < cellnumber; i++) {
			cell[i][5] = cell[i][5] * MeanActivity * labelcellnum / sum1 * Tau;
		}
		return cell;
	}
}
