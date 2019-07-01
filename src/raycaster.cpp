/***************************************************************************
 *   Copyright (C) 2005 by Jeff Ferr                                       *
 *   root@sat                                                              *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include "jgui/japplication.h"
#include "jgui/jwindow.h"
#include "jgui/jraster.h"
#include "jgui/jindexedimage.h"
#include "jgui/jbufferedimage.h"
#include "jmedia/jplayermanager.h"
#include "jmedia/jaudiomixercontrol.h"

#include <algorithm>
#include <numeric>
#include <random>

#define SCREEN_WIDTH 720
#define SCREEN_HEIGHT 480

#define SCALING_MAP 64.0f

#define PLAYER_STEP 5.0f
#define PLAYER_FOV (M_PI/3.0f)
#define PLAYER_ROTATE 0.1f

#define FIRE_SCREEN_WIDTH 180*3
#define FIRE_SCREEN_HEIGHT 75

static uint32_t palette[37] = {
  0xff070707, 0xff1f0707, 0xff2f0f07, 0xff470f07, 
  0xff571707, 0xff671f07, 0xff771f07, 0xff8f2707, 
  0xff9f2f07, 0xffaf3f07, 0xffbf4707, 0xffc74707, 
  0xffdf4f07, 0xffdf5707, 0xffdf5707, 0xffd75f07, 
  0xffd75f07, 0xffd7670f, 0xffcf6f0f, 0xffcf770f, 
  0xffcf7f0f, 0xffcf8717, 0xffc78717, 0xffc78f17, 
  0xffc7971f, 0xffbf9f1f, 0xffbf9f1f, 0xffbfa727, 
  0xffbfa727, 0xffbfaf2f, 0xffb7af2f, 0xffb7b72f, 
  0xffb7b737, 0xffcfcf6f, 0xffdfdf9f, 0xffefefc7, 
  0xffffffff
};

static uint32_t 
  engine_clock = 0;

static uint8_t 
  buffer[FIRE_SCREEN_HEIGHT*FIRE_SCREEN_WIDTH];

class Barrier {

  private:
    jgui::Image 
      *_image;
    jgui::jline_t<int> 
      _line;
    jgui::jpoint_t<int>
      _center;
    float
      _radius;
		float
			_texture_scale;
    bool
      _is_line;

  public:
    Barrier(jgui::Image *image, jgui::jpoint_t<int> p0, jgui::jpoint_t<int> p1)
    {
      _image = image;
      _line = {p0, p1};
			_texture_scale = 1.0f;
      _is_line = true;
    }

    Barrier(jgui::Image *image, jgui::jpoint_t<int> p0, float radius)
    {
      _image = image;
      // _line = {p0, p1};
      _center = p0;
      _radius = radius;
			_texture_scale = 1.0f;
      _is_line = false;
    }

    virtual ~Barrier()
    {
    }

    std::pair<float, jgui::jpoint_t<int>> Intersection(jgui::jline_t<float> line)
    {
      if (_is_line) {
        std::optional<std::pair<float, float>>
          tu = _line.Intersection(line);

        if (tu == std::nullopt or tu->first < 0.0f or tu->first > 1.0f or tu->second < 0.0f) {
          return {-1.0f, {0, 0}};
        }

        return {tu->first, _line.Point(tu->first)};
      }

      std::optional<std::pair<jgui::jpoint_t<float>, jgui::jpoint_t<float>>>
        points = jgui::jcircle_t<float>{_center, _radius}.Intersection(line);

      if (points != std::nullopt) {
        float
          angle = (points->second - _center).Angle();

        // INFO:: hack to avoid a concave version of circle behind of player
        if ((line.p0 - points->first).Norm() > (line.p0 - points->second).Norm()) {
          return {angle/(2*M_PI), points->second};
        }
      }
          
      return {-1.0f, {0, 0}};
    }

    float Distance(jgui::jpoint_t<float> point)
    {
      if (_is_line) {
        std::optional<float>
          t = _line.PerpendicularIntersection(point);

        if (t == std::nullopt or t < 0.0f or t > 1.0f) {
          return NAN;
        }

        return _line.Point(*t).Distance(point);
      }

      return _center.Distance(point) - _radius;
    }

    float GetSize()
    {
      if (_is_line) {
        return _line.Size();
      }

      return 2*M_PI*_radius;
    }

    void Paint(jgui::Raster &raster)
    {
      raster.SetColor(0xffff0000);

      if (_is_line) {
        raster.DrawLine(_line.p0, _line.p1);

        return;
      }

      raster.DrawCircle(_center, _radius);
    }

    void SetTexture(jgui::Image *image)
    {
      _image = image;
    }

    jgui::Image * GetTexture()
    {
      return _image;
    }

		void SetTextureScale(float param)
		{
			_texture_scale = 1.0f/param;

			if (_texture_scale < 0.0f) {
				_texture_scale = 0.0f;
			}
		}

		float GetTextureScale()
		{
			return _texture_scale;
		}

};

class Sprite {

  private:
    std::vector<jgui::Image *>
      _frames;
    jgui::jpoint_t<int>
      _pos;
    jgui::jpoint_t<float>
      _dir;
		float
			_opacity;
    bool
      _alive;

  public:
    Sprite(jgui::Image *image, int frames, jgui::jpoint_t<int> pos, jgui::jpoint_t<float> dir)
    {
      jgui::jsize_t<int>
        size = image->GetSize();
      int 
        step = size.width/frames;

      for (int i=0; i<frames; i++) {
        _frames.push_back(image->Crop({i*step, 0, step, size.height}));
      }

      _pos = pos;
      _dir = dir;
			_opacity = 1.0f;
      _alive = true;
    }

    virtual ~Sprite()
    {
      for (int i=0; i<(int)_frames.size(); i++) {
        // jgui::Image *image = _frames[i];

        // delete image;
      }
    }

    void Invalidate()
    {
      _alive = false;
    }

    bool Valid()
    {
      return _alive;
    }

    jgui::jpoint_t<int> GetPosition()
    {
      return _pos;
    }

    jgui::Image * GetTexture()
    {
      return _frames[(engine_clock/2)%_frames.size()];
    }

		void SetOpacity(float opacity)
		{
			_opacity = opacity;
		}

		float GetOpacity()
		{
			return _opacity;
		}

    void Update()
    {
      _pos += _dir;

      if (_pos.x < 0 or _pos.y < 0 or _pos.x > SCREEN_WIDTH or _pos.y > SCREEN_HEIGHT) {
        Invalidate();
      }
    }

    void Paint(jgui::Raster &raster)
    {
      jgui::Image *scale = GetTexture()->Scale({16, 16});

      raster.DrawImage(scale, {_pos.x - 8, _pos.y - 8});

      delete scale;
    }

};

class Player {

  private:
    std::vector<jgui::Image *>
      _frames;
		jgui::jpoint_t<int>
			_pos;
    float
			_dir,
      _fov;

  public:
    Player(float fov)
    {
			_pos = {
				0, 0
			};

			_dir = 0.0f;
      _fov = fov;

      jgui::BufferedImage 
        image("images/candle.png");
      jgui::jsize_t<int>
        isize = image.GetSize();
      int
        frames = 4,
        step = isize.width/frames;

      for (int i=0; i<frames; i++) {
        jgui::Image *crop = image.Crop({i*step, 0, step, step});

        _frames.push_back(crop->Scale({step*frames, step*frames}));

        delete crop;
      }
    }

    virtual ~Player()
    {
      for (int i=0; i<(int)_frames.size(); i++) {
        jgui::Image *image = _frames[i];

        delete image;
      }
    }

    float GetFieldOfView()
    {
      return _fov;
    }

    void SetDirection(float dir)
    {
			_dir = dir;;
    }

    float GetDirection()
    {
			return _dir;
    }

    jgui::jpoint_t<int> GetPosition()
    {
			return _pos;
    }

    void SetPosition(jgui::jpoint_t<int> point)
    {
			_pos = point;
    }

    void LookAt(float angle)
    {
			_dir = _dir + angle;

      if (_dir < 0.0f) {
        _dir = _dir + 2.0f*M_PI;
      }

      _dir = fmod(_dir, 2.0f*M_PI);
    }

		void Step(int signal, float angle)
		{
      SetPosition(_pos + jgui::jpoint_t<float>{signal*PLAYER_STEP*cos(_dir + angle), signal*PLAYER_STEP*sin(_dir + angle)});
		}

    void Left()
    {
			Step(+1, -M_PI/2.0f);
    }

    void Right()
    {
			Step(+1, M_PI/2.0f);
    }

    void Forward()
    {
			Step(+1, 0.0f);
    }

    void Backward()
    {
			Step(-1, 0.0f);
    }

    void Paint(jgui::Raster &raster)
    {
      static float
        arc = 0.0f;
      static int
        range = 32,
        pos = 0,
        dir = +1;

      arc = fmod(arc + 0.1, 2*M_PI);
      pos = pos + dir*(random()%3);

      if (pos < -range) {
        pos = -range;
        dir = 1;
      }

      if (pos > range) {
        pos = range;
        dir = -1;
      }

      int size = std::min(SCREEN_WIDTH, SCREEN_HEIGHT)/2;

      raster.DrawImage(_frames[(engine_clock/2)%_frames.size()], {(SCREEN_WIDTH - size)/2 + pos, (int)(SCREEN_HEIGHT - 0.9f*size + 0.1f*size*sin(arc))});
    }

};

class Scene : public jgui::Window {

  private:
    std::map<std::string, jgui::Image *>
      _images;
    jmedia::Player 
      *_media;
    jmedia::AudioMixerControl 
      *_control;
    jmedia::Audio 
      *_music;
		jgui::Image
			*_scene;
    std::vector<Barrier> 
      _barriers;
    std::vector<Sprite> 
      _sprites;
    Player 
      _player;
    float
      _zbuffer[SCREEN_WIDTH];
    bool
      _show_flat = false,
      _show_map = false;

  public:
    Scene():
      jgui::Window(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT),
      _player(PLAYER_FOV)
    {
			_scene = new jgui::BufferedImage(jgui::JPF_RGB32, {SCREEN_WIDTH, SCREEN_HEIGHT});

			_scene->GetGraphics()->SetAntialias(jgui::JAM_NONE);

      _images["splash"] = new jgui::BufferedImage("images/splash.png");
      _images["wall0"] = new jgui::BufferedImage("images/greystone.png");
      _images["wall1"] = new jgui::BufferedImage("images/skulls.png");
      _images["barrel"] = new jgui::BufferedImage("images/barrel.png");
      _images["ghost"] = new jgui::BufferedImage("images/ghost.png");
      _images["ghost2"] = new jgui::BufferedImage("images/ghost2.png");
      _images["fireball"] = new jgui::BufferedImage("images/fireball.png");
      _images["player"] = new jgui::BufferedImage("images/player.png");

      _barriers.emplace_back(_images["wall0"], jgui::jpoint_t<int>{0, 0}, jgui::jpoint_t<int>{SCREEN_WIDTH, 0});
      _barriers.emplace_back(_images["wall0"], jgui::jpoint_t<int>{SCREEN_WIDTH, 0}, jgui::jpoint_t<int>{SCREEN_WIDTH, SCREEN_HEIGHT});
      _barriers.emplace_back(_images["wall0"], jgui::jpoint_t<int>{SCREEN_WIDTH, SCREEN_HEIGHT}, jgui::jpoint_t<int>{0, SCREEN_HEIGHT});
      _barriers.emplace_back(_images["wall0"], jgui::jpoint_t<int>{0, SCREEN_HEIGHT}, jgui::jpoint_t<int>{0, 0});

      // circle barrier
      _barriers.emplace_back(_images["wall1"], 
          jgui::jpoint_t<int>{(int)(random()%SCREEN_WIDTH), (int)(random()%SCREEN_HEIGHT)}, 30);

      for (int i=0; i<3; i++) {
        _barriers.emplace_back(_images["wall1"], 
            jgui::jpoint_t<int>{(int)(random()%SCREEN_WIDTH), (int)(random()%SCREEN_HEIGHT)}, jgui::jpoint_t<int>{(int)(random()%SCREEN_WIDTH), (int)(random()%SCREEN_HEIGHT)});
      }
      
      for (int i=0; i<10; i++) {
        jgui::jpoint_t<int>
          pos = {
            (int)(random()%SCREEN_WIDTH), (int)(random()%SCREEN_HEIGHT)
          };

        if (random()%2) {
          _sprites.emplace_back(_images["ghost"], 4, pos, jgui::jpoint_t<float>{0, 0});
				
          _sprites.rbegin()->SetOpacity(0.5f);
        } else {
          _sprites.emplace_back(_images["ghost2"], 6, pos, jgui::jpoint_t<float>{0, 0});
        }
      }

      _player.SetPosition(jgui::jpoint_t<int>{200, 250});
      
      // INFO:: fire
      srand(time(NULL));

      for (int j=0; j<FIRE_SCREEN_HEIGHT; j++) {
      	for (int i=0; i<FIRE_SCREEN_WIDTH; i++) {
					buffer[j*FIRE_SCREEN_WIDTH + i] = 36;
				}
      }

      // INFO:: init sound system
      _control = nullptr;
      _music = nullptr;

      _media= jmedia::PlayerManager::CreatePlayer("mixer://");

      if (_media == nullptr) {
        return;
      }

      _control = dynamic_cast<jmedia::AudioMixerControl *>(_media->GetControl("audio.mixer"));

      if (_control == nullptr) {
        return;
      }

      _music = _control->CreateAudio("sounds/ambiance.wav");

      if (_music == nullptr) {
        return;
      }
      
      _music->SetLoopEnabled(true);
      _control->StartSound(_music);
    }

    virtual ~Scene()
    {
      _barriers.clear();

      for (auto pair : _images) {
        jgui::Image *image = pair.second;

        delete image;
        image = nullptr;
      }

      if (_media != nullptr) {
        _media->Stop();
        _media->Close();
      }

      if (_music != nullptr) {
        delete _music;
        _music = nullptr;
      }
    }

		virtual bool KeyPressed(jevent::KeyEvent *event)
		{
      if (event->GetSymbol() == jevent::JKS_F1) {
        _show_map = !_show_map;
      } else if (event->GetSymbol() == jevent::JKS_F2) {
        _show_flat = !_show_flat;
      }

      return true;
    }

    void KeyHandle()
    {
      static int 
        fire_wait = 0;

      fire_wait = fire_wait - 1;

      jgui::EventManager *ev = GetEventManager();

      if (ev->IsKeyDown(jevent::JKS_SPACE)) { // INFO:: fire
        if (fire_wait < 0) {
          // TODO:: create an animated sprite with direction
          jgui::jpoint_t<float>
            dir {cos(_player.GetDirection()), sin(_player.GetDirection())};
          jgui::jpoint_t<int>
            pos = _player.GetPosition() + dir*50;

          _sprites.emplace_back(_images["fireball"], 24, pos, dir*25);

          fire_wait = 10;
        }
      }

      if (ev->IsKeyDown(jevent::JKS_CURSOR_LEFT)) {
        _player.LookAt(-PLAYER_ROTATE);
      }
      
      if (ev->IsKeyDown(jevent::JKS_CURSOR_RIGHT)) {
        _player.LookAt(PLAYER_ROTATE);
      }
      
      if (
          ev->IsKeyDown(jevent::JKS_CURSOR_UP) or
          ev->IsKeyDown(jevent::JKS_CURSOR_DOWN) or
          ev->IsKeyDown(jevent::JKS_w) or
          ev->IsKeyDown(jevent::JKS_s) or
          ev->IsKeyDown(jevent::JKS_a) or
          ev->IsKeyDown(jevent::JKS_d)) {
        jgui::jpoint_t<int> pos = _player.GetPosition();

        if (ev->IsKeyDown(jevent::JKS_w) or ev->IsKeyDown(jevent::JKS_CURSOR_UP)) {
          _player.Forward();
        } else if (ev->IsKeyDown(jevent::JKS_s) or ev->IsKeyDown(jevent::JKS_CURSOR_DOWN)) {
          _player.Backward();
        } else if (ev->IsKeyDown(jevent::JKS_a)) {
          _player.Left();
        } else if (ev->IsKeyDown(jevent::JKS_d)) {
          _player.Right();
        }

        for (auto barrier : _barriers) {
          if (barrier.Distance(_player.GetPosition()) < 8.0f) { // INFO:: minumum perpendicular distance to wall
            _player.SetPosition(pos);

            return;
          }
        }
      }
    }

    void Framerate(int fps)
    {
      static auto begin = std::chrono::steady_clock::now();
      static int index = 0;

      std::chrono::time_point<std::chrono::steady_clock> timestamp = begin + std::chrono::milliseconds(index++*(1000/fps));
      std::chrono::time_point<std::chrono::steady_clock> current = std::chrono::steady_clock::now();
      std::chrono::milliseconds diff = std::chrono::duration_cast<std::chrono::milliseconds>(timestamp - current);

      if (diff.count() < 0) {
        return;
      }

      std::this_thread::sleep_for(diff);
    }

    void PaintSprites(jgui::Raster &raster)
    {
      // INFO:: draw sprites
      jgui::jpoint_t<int>
        ppos = _player.GetPosition();

      std::sort(_sprites.begin(), _sprites.end(), [&](Sprite &s1, Sprite &s2) {
        return ppos.Distance(s1.GetPosition()) > ppos.Distance(s2.GetPosition());
      });

      for (auto &sprite : _sprites) {
        jgui::jpoint_t<int>
          dpos = sprite.GetPosition() - ppos;
        float
          object_angle = atanf(dpos.y/(float)dpos.x) - _player.GetDirection();

        if (dpos.x < 0) {
          object_angle = object_angle + M_PI;
        }

        object_angle = fmod(object_angle + _player.GetFieldOfView()/2, 2.0f*M_PI);

        if (object_angle < 0.0f) {
          object_angle = object_angle + 2.0f*M_PI;
        }

        object_angle = object_angle - _player.GetFieldOfView()/2.0f;

        bool
          inside_player_view = fabs(object_angle) < ((_player.GetFieldOfView() + M_PI/6.0f)/2.0f); // OPTIMIZE:: process only sprites in field of view (increase field of view to capture unbounded sprites)

        if (inside_player_view) {
          jgui::Image
            *image = sprite.GetTexture();
          jgui::jsize_t<int>
            size = image->GetSize();

          float
            object_distance = dpos.EuclidianNorm(),
            object_ceiling = SCREEN_HEIGHT/2.0f - SCREEN_HEIGHT/(2.0f*object_distance),
            object_floor = SCREEN_HEIGHT - object_ceiling,
            object_height = (object_floor - object_ceiling)*size.height,
            object_ratio = size.height/(float)size.width,
            object_width = object_height/object_ratio,
            object_center = (0.5f * (object_angle/(_player.GetFieldOfView()/2.0f)) + 0.5f) * SCREEN_WIDTH;

          for (int j=0; j<object_height; j++) {
						int y = object_ceiling + j;

						if (y < 0) {
							continue;
						}

						if (y >= SCREEN_HEIGHT) {
							break;
						}

          	for (int i=0; i<object_width; i++) {
              float 
                sample_x = i/object_width,
                sample_y = j/object_height;
              int
                x = (int)(object_center + i - object_width/2.0f);

							if (x < 0) {
								continue;
							}

							if (x >= SCREEN_WIDTH) {
								break;
							}

							if (object_distance <= _zbuffer[x]) {
								uint32_t
									pixel = image->GetGraphics()->GetRGB({(int)(sample_x*size.width), (int)(sample_y*size.height)});

                if (pixel & 0xff000000) {
                  float
                    light = 1.0f - object_distance/std::max(SCREEN_WIDTH, SCREEN_HEIGHT);
                  uint8_t
                    pr = (pixel >> 0x10) & 0xff,
                    pg = (pixel >> 0x08) & 0xff,
                    pb = (pixel >> 0x00) & 0xff;

                  pr = pr*light;
                  pg = pg*light;
                  pb = pb*light;

                  raster.SetColor(0xff000000 | pr << 0x10 | pg << 0x08 | pb);
                  raster.SetPixel({x, y});
                }
							}
            }
          }
        }

        sprite.Update();
      }
    
      // INFO:: remove invalid sprites, like dead persons, fireballs, ...
      _sprites.erase(std::remove_if(_sprites.begin(), _sprites.end(), 
            [](Sprite &param) {
              return param.Valid() == false;
            }), _sprites.end());
    }

    void PaintFire(jgui::Raster &raster)
    {
      for (int j=0; j<FIRE_SCREEN_HEIGHT - 1; j++) {
        for (int i=0; i<FIRE_SCREEN_WIDTH; i++) {
          int decay = random()%3;
          int intensity = buffer[(j + 1)*FIRE_SCREEN_WIDTH + i] - decay;

          if (intensity < 0) {
            intensity = 0;
          }

          buffer[j*FIRE_SCREEN_WIDTH + (i + decay)] = intensity;
        }
      }

      jgui::IndexedImage 
        image(palette, 37, (uint8_t *)buffer, {FIRE_SCREEN_WIDTH, FIRE_SCREEN_HEIGHT});
      jgui::Image
        *crop = image.Crop({(int)(FIRE_SCREEN_WIDTH*_player.GetDirection()/(2.0f*M_PI)), 0, FIRE_SCREEN_WIDTH/3, FIRE_SCREEN_HEIGHT}),
        *scale = crop->Scale({SCREEN_WIDTH, SCREEN_HEIGHT/2});

      raster.DrawImage(scale, {0, 0});

      delete scale;
      delete crop;
    }

    void PaintPlayer(jgui::Raster &raster)
    {
      _player.Paint(raster);
    }

    void PaintMap(jgui::Raster &raster)
    {
      jgui::Image 
        *rotate = _images["player"]->Rotate(-_player.GetDirection() + M_PI);
      jgui::jpoint_t<int>
        pos = _player.GetPosition();
      jgui::jsize_t<int>
        size = rotate->GetSize();
      float 
        arc0 = -_player.GetDirection() + _player.GetFieldOfView()/2.0f,
        arc1 = -_player.GetDirection() - _player.GetFieldOfView()/2.0f;
			int 
				color = 0x80 + random()%64 - 32;

      raster.SetColor(0xff000000 | color << 0x10 | color << 0x08 | color);
      raster.FillArc(pos, {100, 100}, arc1, arc0);
      raster.DrawImage(rotate, {pos.x - size.width/2, pos.y - size.height/2});

      delete rotate;

      for (auto &barrier : _barriers) {
        barrier.Paint(raster);
      }

      for (auto &sprite : _sprites) {
        sprite.Paint(raster);
      }
    }

    void Paint(jgui::Graphics *g) 
    {
      jgui::Raster raster((uint32_t *)cairo_image_surface_get_data(_scene->GetGraphics()->GetCairoSurface()), _scene->GetSize());

      raster.Clear();

      PaintFire(raster);

			// INFO:: key handling
      KeyHandle();

      // INFO:: draw walls
			jgui::jpoint_t<int>
				pos = _player.GetPosition();
      int 
				random_light = random()%10;

      // INFO:: 3d map
      for (int i=0; i<SCREEN_WIDTH; i++) {
        std::pair<float, jgui::jpoint_t<int>> 
          best = {-1.0f, {9999, 9999}};
        Barrier 
          *pbarrier = nullptr;
				float
					angle = -_player.GetFieldOfView()/2.0f + i*_player.GetFieldOfView()/SCREEN_WIDTH + _player.GetDirection();
        float 
          d0 = (pos - best.second).Norm();

        // circle
        for (auto &barrier : _barriers) {
          jgui::jline_t<float>
            ray = {pos, jgui::jpoint_t<float>(pos) + jgui::jpoint_t<float>{cos(angle), sin(angle)}};
          std::pair<float, jgui::jpoint_t<int>> 
            intersection = barrier.Intersection(ray);

          if (intersection.first >= 0.0f) {
            float
              d1 = (pos - intersection.second).Norm();

            if (d1 < d0) {
              best = intersection;
              d0 = d1;
              pbarrier = &barrier;
            }
          }
        }

        if (pbarrier == nullptr) {
          continue;
        }

        d0 = sqrtf(d0);

        _zbuffer[i] = d0;

        float
          cosf = cos(-_player.GetFieldOfView()/2.0f + (i*_player.GetFieldOfView())/(float)SCREEN_WIDTH);
        int
          wall = (SCREEN_HEIGHT*SCALING_MAP)/(d0*cosf);

        if (_show_flat == true) {
          raster.SetColor(0xfff0f0f0);
          raster.DrawLine({i, SCREEN_HEIGHT/2 - wall}, {i, SCREEN_HEIGHT/2 + wall});
        } else {
          jgui::Image 
            *texture = pbarrier->GetTexture();
          jgui::jsize_t<int>
            tsize = texture->GetSize();
					float
						scale = pbarrier->GetTextureScale();
          int 
            index = (int)(best.first*tsize.width);

					if (scale != 0.0f) {
            index = (int)(best.first*pbarrier->GetSize()*scale)%tsize.width;
					}

          // INFO:: casting walls
          for (int j=SCREEN_HEIGHT/2 - wall; j<SCREEN_HEIGHT/2 + wall; j++) {
            if (j > 0 and j < SCREEN_HEIGHT) {
              int 
								size = j - SCREEN_HEIGHT/2 + wall;

              raster.SetColor(texture->GetGraphics()->GetRGB({index, (tsize.height*size)/(2*wall)}));
              raster.SetPixel({i, j});
            }
          }

          // INFO:: floor with lights/shadows
          int wall_limit = SCREEN_HEIGHT/2 + wall;

          if (wall_limit > SCREEN_HEIGHT) {
            wall_limit = SCREEN_HEIGHT;
          }

          // fire light on floor
          for (int j=wall_limit; j<SCREEN_HEIGHT; j++) {
            float
              d = SCREEN_HEIGHT/(2.0f*j - SCREEN_HEIGHT)/cosf;
            int 
              c = 0xff - (0x80 + random_light)*d;

            if (c < 0x00) {
              c = 0x00;
            }
            
            if (c > 0xff) {
              c = 0xff;
            }

            raster.SetColor(0xff000000 | c << 0x10 | c << 0x08 | c << 0x00);
            raster.SetPixel({i, j});
          }
        }
      }

      PaintSprites(raster);

      PaintPlayer(raster);

      if (_show_map == true) {
        PaintMap(raster);
      }

      // INFO:: splash screen
      static int splash_timer = 0;

      if (splash_timer++ < 100) {
        _scene->GetGraphics()->DrawImage(_images["splash"], {0, 0, _scene->GetSize()});
      }

			g->DrawImage(_scene, {{0, 0}, GetSize()});

      engine_clock++;

      Repaint();

      Framerate(25);
    }

};

int main(int argc, char **argv)
{
  jgui::Application::Init(argc, argv);

  srandom(time(NULL));

  Scene app;

  app.SetTitle("Scene");
  app.Exec();

  jgui::Application::Loop();

  return 0;
}
