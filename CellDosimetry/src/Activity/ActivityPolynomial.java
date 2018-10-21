package Activity;

import java.util.Random;

/**
 * Created by ar548 on 9/13/2016.
 */
public class ActivityPolynomial {
	public static double[][] generateActivity(
			int cellnumber, int Radius, int Height, int labelcellnum, int degree,
			double[] coefficients,
			String Shape,
			double MeanActivity, double Tau,
			double[][] cell
	){
		int TMC = 0, j;
		double sum1 = 0.0, rToCell, rho;
		Random randomgen = new Random();

		for(int i = 0; i < labelcellnum; i++) {
			j = randomgen.nextInt(cellnumber);
			if(cell[j][4] != 0){
				i--;
				continue;
			}
			cell[j][4] = 1; // this cell is labeled

			rToCell = Math.sqrt( Math.pow( cell[j][1], 2 ) + Math.pow( (cell[j][2]), 2 ) + Math.pow( cell[j][3], 2 ) );
			rho = 0.0;
			for(int k = degree; k >= 0; k--) {
				rho += coefficients[k]*Math.pow( rToCell, k );
			}
			rho = (rho >= 0) ? rho : 0;
			cell[j][5] = rho;
			sum1 += cell[j][5];

		}

		for(int i = 0; i < cellnumber; i++) {
			cell[i][5] = cell[i][5] * MeanActivity * labelcellnum / sum1 * Tau;
		}

		return cell;
	}
}
