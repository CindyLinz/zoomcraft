#version 130

in vec2 UV;
uniform sampler2D tex;

void main(){
  //gl_FragColor = texture(tex, UV);
  gl_FragColor = vec4(texture(tex, UV).rgb, 1);
  //gl_FragColor = vec4(1,0,0,1);
  //gl_FragColor = mix(texture(tex, UV), vec4(1,1,0,1), 0.5);
}
