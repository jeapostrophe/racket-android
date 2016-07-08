package org.racketlang.android.project;

import android.app.Activity;
import android.widget.TextView;
import android.os.Bundle;

public class RacketAndroidProject extends Activity
{
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

        TextView tv = new TextView(this);
        tv.setText( go() );
        setContentView(tv);
    }

    public native String go();

    static {
        System.loadLibrary("racket-android-project");
    }
}
