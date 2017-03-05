package org.racketlang.android.project;

import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.concurrent.Semaphore;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import android.app.Activity;
import android.widget.TextView;
import android.os.Bundle;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.IntentSender.SendIntentException;
import android.graphics.PixelFormat;
import android.opengl.GLSurfaceView;
import android.util.AttributeSet;
import android.util.Log;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.View;

import android.content.res.Resources;
import android.content.res.AssetFileDescriptor;
import android.content.res.AssetManager;
import android.media.MediaPlayer;

import javax.microedition.khronos.egl.EGL10;
import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.egl.EGLContext;
import javax.microedition.khronos.egl.EGLDisplay;
import javax.microedition.khronos.opengles.GL10;

import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.drive.Drive;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.DriveApi;
import com.google.android.gms.drive.DriveApi.MetadataBufferResult;
import com.google.android.gms.drive.DriveApi.DriveContentsResult;
import com.google.android.gms.drive.DriveFile;
import com.google.android.gms.drive.DriveFolder;
import com.google.android.gms.drive.DriveFolder.DriveFileResult;
import com.google.android.gms.drive.DriveResource;
import com.google.android.gms.drive.DriveContents;
import com.google.android.gms.drive.Metadata;
import com.google.android.gms.drive.MetadataBuffer;
import com.google.android.gms.drive.MetadataChangeSet;
import com.google.android.gms.drive.query.Query;
import com.google.android.gms.drive.query.SearchableField;
import com.google.android.gms.drive.query.Filter;
import com.google.android.gms.drive.query.Filters;

class RLib {
    public static native void onDrawFrame();
    public static native void onSurfaceChanged(int w, int h);
    public static native void onSurfaceCreated();
    public static native void onCreate( RAPAudio rs, RAPDrive rd );
    public static native boolean onTouchEvent(int a, float x, float y);
    public static native void soundComplete( int id );

    public static final int DRIVE_STATUS_OK = 0;
    public static final int DRIVE_STATUS_ERROR_CONNECT = 1;
    public static final int DRIVE_STATUS_ERROR_AUTH = 2;
    public static final int DRIVE_STATUS_ERROR_SUSPENDED = 2;
    public static native void setDriveStatus( int m );

    static {
        System.loadLibrary("racket-android-project");
    }
}

class RAPView extends GLSurfaceView {
    private static final String TAG = "RacketAndroidProject";
    private static final boolean DEBUG = true;

    public RAPView(Context context) {
        super(context);
        // Pick an EGLConfig with RGB8 color, 16-bit depth, no stencil,
        // supporting OpenGL ES 3.0 or later backwards-compatible versions.
        setEGLConfigChooser(8, 8, 8, 0, 16, 0);
        setEGLContextClientVersion(3);
        setRenderer(new Renderer());
        setRenderMode(RENDERMODE_CONTINUOUSLY);
    }

    private static class Renderer implements GLSurfaceView.Renderer {
        public void onDrawFrame(GL10 gl) {
            RLib.onDrawFrame();
        }

        public void onSurfaceChanged(GL10 gl, int width, int height) {
            RLib.onSurfaceChanged(width, height);
        }

        public void onSurfaceCreated(GL10 gl, EGLConfig config) {
            RLib.onSurfaceCreated();
        }
    }
}

class RAPListener implements View.OnTouchListener {
    public boolean onTouch(View v, MotionEvent event) {
        RLib.onTouchEvent(event.getAction(), event.getX(), event.getY());
        return true;
    }
}

class RAPAudio {
    static class Request {
        public String p;
        public int id;
        public Request( String p, int id ) {
            this.p = p;
            this.id = id;
        }
    }

    class TheWorker implements Runnable {
        class TheListener implements MediaPlayer.OnCompletionListener {
            Semaphore signal = null;

            public TheListener( Semaphore signal ) {
                this.signal = signal;
            }

            public void onCompletion( MediaPlayer mp ) {
                Log.w("RAPAudio", "Outstanding play finished");
                this.signal.release();
            }
        }

        BlockingQueue<Request> bq = null;

        Semaphore signal = null;
        TheListener cl = null;
        AssetManager am = null;
        MediaPlayer mp = null;

        public TheWorker( Context context, BlockingQueue<Request> bq ) {
            this.bq = bq;
            this.signal = new Semaphore(0);
            this.am = context.getResources().getAssets();
            this.mp = new MediaPlayer();
            this.cl = new TheListener(signal);

            this.mp.setOnCompletionListener( this.cl );
        }

        void doPlaySound( String p ) {
            Log.w("RAPAudio", "Starting to load sample (" + p + ")");
            try {
                AssetFileDescriptor afd = am.openFd(p);
                mp.setDataSource(afd.getFileDescriptor(),
                                 afd.getStartOffset(),
                                 afd.getLength());
                afd.close();
                mp.prepare();
            }
            catch (IOException e) {
                Log.e("RAPAudio", "Failed to load sample (" + p + ")");
            }

            Log.w("RAPAudio", "Playing sample (" + p + ")");
            mp.start();
        }

        public void run() {
            while (true) {
                mp.reset();
                try {
                    Request r = bq.take();
                    Log.w("RAPAudio", "Dequeued sample request (" + r.p + ")");
                    doPlaySound( r.p );
                    signal.acquire();
                    RLib.soundComplete( r.id );
                }
                catch (InterruptedException e) {
                    Log.e("RAPAudio", "Failed to dequeue sample");
                }
            }
        }
    }

    static TheWorker tw = null;
    static BlockingQueue<Request> bq = null;
    static Thread twt = null;
    static int next_id = 0;
    public RAPAudio(Context context) {
        bq = new LinkedBlockingQueue<Request>(128);
        tw = new TheWorker( context, bq );
        twt = new Thread(tw);

        twt.start();
    }

    static int playSound( String p ) {
        int id = next_id++;
        Request r = new Request( p, id );

        Log.w("RAPAudio", "Queuing sample request (" + p + ")");
        try {
            bq.put(r);
        }
        catch (InterruptedException e) {
            Log.e("RAPAudio", "Failed to queue sample (" + p + ")");
        }
        Log.w("RAPAudio", "Queued sample request (" + p + ")");

        return id;
    }
}

class RAPDrive {
    public static GoogleApiClient gac = null;

    static DriveFile maybeCreateOnDrive( DriveFolder df, String p, boolean create_p ) {
        Log.e("RAPDrive.maybeCreateOnDrive", "maybeCreate (" + p + ")");
        if ( ! create_p ) {
            Log.e("RAPDrive.maybeCreateOnDrive", "Not creating (" + p + ")");
            return null;
        }
        MetadataChangeSet mcs = new MetadataChangeSet.Builder()
            .setTitle( p )
            .build();
        DriveFileResult dfr = df.createFile( gac, mcs, null, null ).await();

        return dfr.getDriveFile();
    }

    static DriveFile openOnDrive( String p, boolean create_p ) {
        Log.e("RAPDrive.openOnDrive", "trying to open (" + p + ")");
        DriveFolder af = Drive.DriveApi.getAppFolder( gac );
        Query q = new Query.Builder()
            .addFilter(Filters.eq(SearchableField.TITLE, p))
            .build();
        MetadataBuffer md = af.queryChildren( gac, q ).await().getMetadataBuffer();;
        if ( md.getCount() < 1 ) {
            Log.e("RAPDrive.openOnDrive", "No search result for (" + p + ")");
            return maybeCreateOnDrive( af, p, create_p );
        }
        Metadata m = md.get(0);
        DriveId di = m.getDriveId();
        md.release();
        DriveFile df = di.asDriveFile();

        return df;
    }

    static String read( String p ) {
        Log.e("RAPDrive.read", "about to read, p.len = " + p.length());
        Log.e("RAPDrive.read", "trying to read (" + p + ")");
        DriveFile df = openOnDrive( p, false );
        if ( df == null ) { return null; }

        DriveContentsResult dcr = df.open( gac, DriveFile.MODE_READ_ONLY, null ).await();
        if ( ! dcr.getStatus().isSuccess() ) {
            Log.e("RAPDrive.read", "DriveFile open failed (" + p + ")");
            return null;
        }
        DriveContents dc = dcr.getDriveContents();
        BufferedReader br =
            new BufferedReader(new InputStreamReader(dc.getInputStream()));
        StringBuilder b = new StringBuilder();
        boolean status = true;
        try {
            String l;
            while ((l = br.readLine()) != null) {                
                b.append(l);
            }
        } catch (IOException e) {
            Log.e("RAPDrive.read", "Failed to read (" + p + ")");
            status = false;
        }
        dc.discard(gac);

        if ( status ) {
            String contents = b.toString();
            Log.e("RAPDrive.read", "Success while reading (" + p + ")");
            return contents;
        } else {
            Log.e("RAPDrive.read",
                  "Failed at some point, so returning null (" + p + ")");
            return null;
        }
    }

    static boolean write( String p, String c ) {
        Log.e("RAPDrive.write", "trying to write (" + p + ")");
        DriveFile df = openOnDrive( p, true );
        if ( df == null ) {
            Log.e("RAPDrive.write", "Failed to locate or create file (" + p + ")");
            return false;
        }

        DriveContentsResult dcr = df.open( gac, DriveFile.MODE_WRITE_ONLY, null ).await();
        if ( ! dcr.getStatus().isSuccess() ) {
            Log.e("RAPDrive.write", "Failed to open DriveFile (" + p + ")");
            return false;
        }
        DriveContents dc = dcr.getDriveContents();

        Writer writer = new OutputStreamWriter(dc.getOutputStream());
        try {
            writer.write(c);
            writer.close();
            dc.commit(gac, null).await();
        } catch (IOException e) {
            Log.e("RAPDrive.write", "Failed while writing (" + p + ")");
            return false;
        }

        Log.e("RAPDrive.write", "Success while writing (" + p + ")");
        return true;
    }
}

public class RacketAndroidProject
    extends Activity
    implements ConnectionCallbacks, OnConnectionFailedListener {

    private static final int REQUEST_CODE_RESOLUTION = 3;

    RAPView mView;
    public GoogleApiClient mGoogleApiClient;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Log.e("RacketAndroidProject.onCreate", "initializing");
        
        RLib.onCreate( new RAPAudio(getApplication()),
                       new RAPDrive() );

        mView = new RAPView(getApplication());
        mView.setOnTouchListener(new RAPListener());
        mView.requestRender();
        setContentView(mView);

        mGoogleApiClient = new GoogleApiClient.Builder(this)
            .useDefaultAccount()
            .addApi(Drive.API)
            .addScope(Drive.SCOPE_FILE)
            .addScope(Drive.SCOPE_APPFOLDER)
            .addConnectionCallbacks(this)
            .addOnConnectionFailedListener(this)
            .build();
        RAPDrive.gac = mGoogleApiClient;
    }

    @Override
    protected void onStart() {
        super.onStart();
        mGoogleApiClient.connect();
    }

    @Override
    public void onConnectionFailed(ConnectionResult connectionResult) {
        if (connectionResult.hasResolution()) {
            try {
                connectionResult.startResolutionForResult(this, REQUEST_CODE_RESOLUTION);
            } catch (IntentSender.SendIntentException e) {
                // Unable to resolve, message user appropriately
                RLib.setDriveStatus( RLib.DRIVE_STATUS_ERROR_CONNECT );
            }
        } else {
            GooglePlayServicesUtil.getErrorDialog(connectionResult.getErrorCode(), this, 0).show();
        }
    }

    @Override
    protected void onActivityResult(final int requestCode, final int resultCode, final Intent data) {
        switch (requestCode) {
        case REQUEST_CODE_RESOLUTION:
            if (resultCode == RESULT_OK) {
                mGoogleApiClient.connect();
            }
            break;
        default:
            RLib.setDriveStatus( RLib.DRIVE_STATUS_ERROR_AUTH );
            break;
        }
    }

    @Override
    public void onConnected(Bundle connectionHint) {
        RLib.setDriveStatus( RLib.DRIVE_STATUS_OK );
    }

    @Override
    public void onPause() {
        super.onPause();
        mView.onPause();
    }

    @Override
    public void onResume() {
        super.onResume();
        mView.onResume();
    }

    @Override
    public void onConnectionSuspended(int cause) {
        RLib.setDriveStatus( RLib.DRIVE_STATUS_ERROR_SUSPENDED );
    }
}
