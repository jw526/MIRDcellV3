package WebDose;

import javax.swing.JFrame;
import javax.swing.JApplet;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

/**
 * Created by Alex Rosen on 7/26/2016.
 */
public class AAMain {
	public static void main(String[] args){
		/*
		 * The whole reason for this is so that Dr howell can test it easier because his netbeans cant run cellApplet for some reas
		 */
		JFrame frame = new JFrame(  );
		frame.setSize( 1096, 749 );
		final JApplet program = new CellApplet();
		frame.getContentPane().add(program);
		frame.addWindowListener( new WindowListener() {
			@Override
			public void windowOpened(WindowEvent e) {

			}

			@Override
			public void windowClosing(WindowEvent e) {
				program.stop();
				program.destroy();
				System.exit(0);
			}

			@Override
			public void windowClosed(WindowEvent e) {
				program.stop();
				program.destroy();
				System.exit(0);
			}

			@Override
			public void windowIconified(WindowEvent e) {

			}

			@Override
			public void windowDeiconified(WindowEvent e) {

			}

			@Override
			public void windowActivated(WindowEvent e) {
			}

			@Override
			public void windowDeactivated(WindowEvent e) {

			}
		} );
		frame.setVisible( true );
		program.init();
		program.start();
	}
}
