#version 130

attribute vec2 pos;
attribute vec2 uv;
out vec2 UV;

void main(){
  gl_Position = vec4(pos, 1, 1);
  UV = uv;
}
