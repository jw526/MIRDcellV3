package WebDose;

import javax.swing.*;
import java.awt.*;

/**
 * Created by Alex on 7/13/2017.
 */
public class EquationsImage{

	public EquationsImage(){
		JFrame frame = new JFrame();
		frame.setSize(new Dimension(821, 759));
		frame.add(new JLabel( new ImageIcon("./Equations.PNG")));
		frame.show();
	}

	public static void main(String[] args){
		EquationsImage e = new EquationsImage();
	}
}

