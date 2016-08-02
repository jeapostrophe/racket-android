package org.racketlang.android.project;

import java.io.IOException;

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
import android.media.SoundPool;

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
        // supporting OpenGL ES 2.0 or later backwards-compatible versions.
        setEGLConfigChooser(8, 8, 8, 0, 16, 0);
        setEGLContextClientVersion(3);
        setRenderer(new Renderer());
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
    // XXX Maybe use a Map to save old sounds, current it task about 100ms to load
    static SoundPool sp = new SoundPool.Builder().build();

    class RAPSPListener implements SoundPool.OnLoadCompleteListener {
        RAPAudio ra = null;
        
        public RAPSPListener( RAPAudio ra ) {
            this.ra = ra;
        }

        public void onLoadComplete( SoundPool sp, int sample, int status ) {
            if ( status == 0 ) {
                ra.finish(sample);
            }
        }
    }
    static RAPSPListener lcl = null;
    
    static AssetManager am = null;
    public RAPAudio(Context context) {
        am = context.getResources().getAssets();
        lcl = new RAPSPListener(this);
        sp.setOnLoadCompleteListener( lcl );
    }

    static boolean playingHuh = false;
    static String loading_p = null;
    static int sample_id = 0;
    static int stream_id = 0;

    static void finish( int finished_sample_id ) {
        if ( sample_id == finished_sample_id ) {
            Log.w("RAPAudio", "Finished loading sample (" + loading_p + ")");
            stream_id = sp.play(sample_id, 1.0f, 1.0f, 0, 0, 1.0f);
            if ( stream_id == 0 ) {
                Log.e("RAPAudio", "Failed to play sample (" + loading_p + ")");
            } else {
                playingHuh = true;
            }
        }
    }
    
    static void playSound( String p ) {
        if ( playingHuh ) {
            sp.stop(stream_id);
            sp.unload(sample_id);
            playingHuh = false;
        }

        loading_p = p;
        try {
            AssetFileDescriptor afd = am.openFd(p);
            Log.w("RAPAudio", "Starting to load sample (" + loading_p + ")");
            sample_id = sp.load(afd, 1);
            // afd.close();
        }
        catch (IOException e) {
            Log.e("RAPAudio", "Failed to load sample (" + loading_p + ")");
        }
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
