package workbenchhelpers;

import java.io.IOException;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin implements IStartup
{
	// The plug-in ID
	public static final String PLUGIN_ID = "WorkbenchHelpers"; //$NON-NLS-1$

	// The shared instance
	private static Activator plugin;
	
	private HttpServer server;
	
	private static boolean isInitialized;
	
	/**
	 * The constructor
	 */
	public Activator() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}
	
	public void initHttpServer()
	{
		if (!isInitialized)
		{
			System.out.println("Initializing Workbench Helpers plugin...");
			
			if (server != null)
			{
				server.stop();
			}

			server = new HttpServer();
			
			try
			{					
				System.out.println("Starting HTTP server...");
				
				server.start();
			}
			catch (IOException e)
			{
				System.out.println("Error initializing WorkbenchHelpers HTTP server: " + e.getMessage());
				e.printStackTrace();
			}
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}

	@Override
	public void earlyStartup()
	{
		initHttpServer();
	}
}
