package org.racketlang.android.project;

import java.io.IOException;
import java.util.concurrent.Semaphore;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import android.app.Activity;
import android.widget.TextView;
import android.os.Bundle;
import android.content.Context;
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

class RLib {
    public static native void onDrawFrame();
    public static native void onSurfaceChanged(int w, int h);
    public static native void onSurfaceCreated();
    public static native void onCreate( RAPAudio rs );
    public static native boolean onTouchEvent(int a, float x, float y);
    public static native void soundComplete( int id );
    
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

public class RacketAndroidProject extends Activity {
    RAPView mView;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        RLib.onCreate( new RAPAudio(getApplication()) );
        mView = new RAPView(getApplication());
        mView.setOnTouchListener(new RAPListener());
        mView.requestRender();
        setContentView(mView);
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
}
