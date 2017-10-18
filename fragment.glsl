#version 330 core

in vec4 pos;

out vec4 color;

uniform sampler2D tex;

void main() {
  if (pos.w < 0)
    color = texture(tex, vec2((fract(pos.x) + pos.w) / 16.0, 1 - pos.z));
  else
    color = texture(tex, vec2((fract(pos.x + pos.z) + pos.w) / 16.0, 1 - pos.y));
}
