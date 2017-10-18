#version 330 core

layout (location = 0) in ivec4 position;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec4 pos;

void main() {
  gl_Position = projection * view * model
    * vec4(float(position.x),float(position.y),float(position.z), 1);
  pos = vec4(float(position.x),float(position.y),float(position.z),float(position.w));
}
