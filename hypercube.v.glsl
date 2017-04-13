#version 330 core
layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoord;
layout (location = 2) in float light;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec3 ourColor;
out vec2 TexCoord;

void main(void) {
  gl_Position = projection * view * model * vec4(position, 1.0);
  ourColor = vec3(light,light,light);
  TexCoord = texCoord;
}
