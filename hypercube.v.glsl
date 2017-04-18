#version 330 core

layout (location = 0) in vec4 position;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec4 pos;

void main() {
  gl_Position = projection * view * model * vec4(position.xyz, 1);
  pos = position;
}
