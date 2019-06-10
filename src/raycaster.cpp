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

#define SCREEN_WIDTH 720
#define SCREEN_HEIGHT 480

#define SCALING_X 1.0f
#define SCALING_Y 2.0f

#define PLAYER_STEP 4.0f
#define PLAYER_FOV 60.0f
#define PLAYER_ROTATE 4.0f

#define FIRE_SCREEN_WIDTH 240*3
#define FIRE_SCREEN_HEIGHT 90

uint32_t palette[37] = {
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

uint8_t buffer[FIRE_SCREEN_HEIGHT][FIRE_SCREEN_WIDTH];

class Barrier {

  private:
    jgui::Image 
      *_image;
    jgui::jline_t<int> 
      _line;

  public:
    Barrier(jgui::jline_t<int> line)
    {
      _line = line;
    }

    virtual ~Barrier()
    {
    }

    jgui::jline_t<int> GetBounds()
    {
      return _line;
    }

    float GetSize()
    {
      int 
        x = _line.p0.x - _line.p1.x,
        y = _line.p0.y - _line.p1.y;

      return sqrtf(x*x + y*y);
    }

    void Paint(jgui::Raster &raster)
    {
      raster.SetColor(0xffff0000);
      raster.DrawLine(_line);
      
      // INFO:: earthquake
      /*
      int 
        x = random()%5 - 2,
        y = random()%5 - 2;

      _line.p0.x += x;
      _line.p0.y += y;
      
      _line.p1.x += x;
      _line.p1.y += y;
      */
    }

    void SetTexture(jgui::Image *image)
    {
      _image = image;
    }

    jgui::Image * GetTexture()
    {
      return _image;
    }

};

class Ray {

  private:
    jgui::jpoint_t<int> _p0;
    jgui::jpoint_t<float> 
      _p1,
      _px;

  public:
    Ray(jgui::jpoint_t<int> p0, float angle)
    {
      angle = 2*M_PI*angle/360.0f;
      
      _p0 = p0;

      float 
        fx = cos(angle),
        fy = sin(angle);
      float
        magnitude = fabs(fx*fx + fy*fy);

      _p1 = {
        .x = fx/magnitude,
        .y = fy/magnitude
      };

      _px = _p1;
    }

    virtual ~Ray()
    {
    }

    jgui::jpoint_t<int> GetPosition() const
    {
      return _p0;
    }

    jgui::jpoint_t<float> GetDirection() const
    {
      return _p1;
    }

    void SetPosition(jgui::jpoint_t<int> point)
    {
      _p0 = point;
    }

    void LookAt(float angle)
    {
      float old = atan2f(_p1.y, _p1.x);

      angle = old - 2*M_PI*angle/360.0f;

      float 
        fx = cos(angle),
        fy = sin(angle);
      float
        magnitude = fabs(fx*fx + fy*fy);

      _p1 = {
        .x = fx/magnitude,
        .y = fy/magnitude
      };
    }

    std::pair<float, jgui::jpoint_t<int>> Cast(Barrier &barrier) const
    {
      jgui::jline_t<int> line = barrier.GetBounds();

      const float x1 = line.p0.x;
      const float y1 = line.p0.y;
      const float x2 = line.p1.x;
      const float y2 = line.p1.y;

      const float x3 = _p0.x;
      const float y3 = _p0.y;
      const float x4 = _p0.x + _p1.x;
      const float y4 = _p0.y + _p1.y;

      const float den = (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4);

      std::pair<float, jgui::jpoint_t<int>> result = std::make_pair(-1.0f, jgui::jpoint_t<int>{-1, -1});

      if (den == 0) {
        return result;
      }

      const float t = ((x1 - x3)*(y3 - y4) - (y1 - y3)*(x3 - x4))/den;
      const float u = -((x1 - x2)*(y1 - y3) - (y1 - y2)*(x1 - x3))/den;

      if (t > 0.0f and t < 1.0f and u > 0.0f) {
        result = std::make_pair(t, jgui::jpoint_t<int>{(int)(x1 + t*(x2 - x1)), (int)(y1 + t*(y2 - y1))});
      }

      return result;
    }

    void Paint(jgui::Raster &raster)
    {
      raster.DrawLine({_p0, {(int)(_p0.x + 10*_p1.x), (int)(_p0.y + 10*_p1.y)}});
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

      jgui::BufferedImage image("images/candle.png");

      for (int i=0; i<4; i++) {
        jgui::Image *crop = image.Crop({i*64, 0, 64, 64});

        _frames.push_back(crop->Scale({64*4, 64*4}));

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
			_dir = fmod(_dir + angle, 360.0f);
    }

    void Forward()
    {
			float 
				rad = M_PI*_dir/180.0f;

      SetPosition({(int)(_pos.x + PLAYER_STEP*cos(rad)), (int)(_pos.y + PLAYER_STEP*sin(rad))});
    }

    void Backward()
    {
			float 
				rad = M_PI*_dir/180.0f;

      SetPosition({(int)(_pos.x - PLAYER_STEP*cos(rad)), (int)(_pos.y - PLAYER_STEP*sin(rad))});
    }

    void Paint(jgui::Raster &raster)
    {
      static int frame = 0;

      int size = std::min(SCREEN_WIDTH, SCREEN_HEIGHT)/2;

      raster.DrawImage(_frames[(frame++/2)%_frames.size()], {(SCREEN_WIDTH - size)/2, SCREEN_HEIGHT - size});
    }

};

class Scene : public jgui::Window {

  private:
    jgui::Image
      *_bricks,
      *_eagles,
      *_torch,
      *_floor;
    std::vector<Barrier> 
      _barriers;
    Player 
      _player;
    int
      _show_flat = false,
      _show_map = false;

  public:
    Scene():
      jgui::Window(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT),
      _player(PLAYER_FOV)
    {
      _bricks = new jgui::BufferedImage("images/redbrick.png");
      _eagles = new jgui::BufferedImage("images/wall01.png");
      // _eagles = new jgui::BufferedImage("images/eagle.png");
      _torch = new jgui::BufferedImage("images/torch.png");
      _floor = new jgui::BufferedImage("images/greystone.png");

      _barriers.emplace_back(jgui::jline_t<int>{{0, 0}, {SCREEN_WIDTH, 0}});
      _barriers.emplace_back(jgui::jline_t<int>{{SCREEN_WIDTH, 0}, {SCREEN_WIDTH, SCREEN_HEIGHT}});
      _barriers.emplace_back(jgui::jline_t<int>{{SCREEN_WIDTH, SCREEN_HEIGHT}, {0, SCREEN_HEIGHT}});
      _barriers.emplace_back(jgui::jline_t<int>{{0, SCREEN_HEIGHT}, {0, 0}});

      for (auto &barrier : _barriers) {
        barrier.SetTexture(_bricks);
      }

      for (int i=0; i<3; i++) {
        jgui::jline_t<int>
          line = {
            {(int)(random()%SCREEN_WIDTH), (int)(random()%SCREEN_HEIGHT)},
            {(int)(random()%SCREEN_WIDTH), (int)(random()%SCREEN_HEIGHT)}
          };

        _barriers.emplace_back(line);

        _barriers.rbegin()->SetTexture(_eagles);
      }
      
      _player.SetPosition(jgui::jpoint_t<int>{200, 250});
      
      // INFO:: fire
      srand(time(NULL));

      for (int j=0; j<FIRE_SCREEN_HEIGHT; j++) {
      	for (int i=0; i<FIRE_SCREEN_WIDTH; i++) {
					buffer[j][i] = 36;
				}
      }

      SetResizable(false);
    }

    virtual ~Scene()
    {
      _barriers.clear();

      delete _torch;
      _torch = nullptr;

      delete _bricks;
      _bricks = nullptr;

      delete _eagles;
      _eagles = nullptr;
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
      jgui::EventManager *ev = GetEventManager();

      if (ev->IsKeyDown(jevent::JKS_CURSOR_LEFT)) {
        _player.LookAt(-PLAYER_ROTATE);
      } else if (ev->IsKeyDown(jevent::JKS_CURSOR_RIGHT)) {
        _player.LookAt(PLAYER_ROTATE);
      } else if (
          ev->IsKeyDown(jevent::JKS_CURSOR_UP) or
          ev->IsKeyDown(jevent::JKS_CURSOR_DOWN)) {
        jgui::jpoint_t<int> pos = _player.GetPosition();

        if (ev->IsKeyDown(jevent::JKS_CURSOR_UP)) {
          _player.Forward();
        } else {
          _player.Backward();
        }
      
      	for (int i=0; i<SCREEN_WIDTH; i+=SCREEN_WIDTH >> 4) {
					Ray 
						ray(_player.GetPosition(), -_player.GetFieldOfView()/2.0f + i*_player.GetFieldOfView()/SCREEN_WIDTH + _player.GetDirection());
          jgui::jpoint_t<int> 
            pray = ray.GetPosition();

          for (auto barrier : _barriers) {
            std::pair<float, jgui::jpoint_t<int>> point = ray.Cast(barrier);

            if (point.first >= 0.0f) {
              int 
                d1 = sqrtf((pray.x - point.second.x)*(pray.x - point.second.x) + (pray.y - point.second.y)*(pray.y - point.second.y));

              if (d1 < 16) { // INFO:: minimal distance to avoid distortions when hit the barrier
                _player.SetPosition(pos);

                return;
              }
            }
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

    void Paint(jgui::Graphics *g) 
    {
      jgui::Raster raster((uint32_t *)cairo_image_surface_get_data(g->GetCairoSurface()), GetSize());

      raster.Clear();

      // INFO:: fire
      jgui::jsize_t<int>
        size = GetSize();
 
      for (int j=0; j<FIRE_SCREEN_HEIGHT - 1; j++) {
        for (int i=0; i<FIRE_SCREEN_WIDTH; i++) {
					int decay = random()%3;
          int intensity = buffer[j + 1][i] - decay;

					if (intensity < 0) {
						intensity = 0;
					}

					buffer[j][i + decay] = intensity;
        }
      }
      
			jgui::IndexedImage image(
				palette, 37, (uint8_t *)buffer, {FIRE_SCREEN_WIDTH, FIRE_SCREEN_HEIGHT});
      int 
        left = (2*FIRE_SCREEN_WIDTH*_player.GetDirection())/360.0;

      g->DrawImage(&image, {left, 0, FIRE_SCREEN_WIDTH/3, FIRE_SCREEN_HEIGHT}, {0, 0, size.width, size.height/2});

			// INFO:: key handling
      KeyHandle();

      // INFO:: draw walls
      int random_light = random()%10;

      // INFO:: 3d map
      for (int i=0; i<SCREEN_WIDTH; i++) {
				Ray ray(_player.GetPosition(), -_player.GetFieldOfView()/2.0f + i*_player.GetFieldOfView()/SCREEN_WIDTH + _player.GetDirection());

        std::pair<float, jgui::jpoint_t<int>> 
          best = {-1.0f, {9999, 9999}};
        Barrier 
          *pbarrier = nullptr;
        jgui::jpoint_t<int> 
          pray = ray.GetPosition();
        int 
          d0 = (pray.x - best.second.x)*(pray.x - best.second.x) + (pray.y - best.second.y)*(pray.y - best.second.y);

        for (auto &barrier : _barriers) {
          std::pair<float, jgui::jpoint_t<int>> point = ray.Cast(barrier);

          if (point.first >= 0.0f) {
            int 
              d1 = (pray.x - point.second.x)*(pray.x - point.second.x) + (pray.y - point.second.y)*(pray.y - point.second.y);

            if (d1 < d0) {
              best = point;
              d0 = d1;
              pbarrier = &barrier;
            }
          }
        }

        if (pbarrier == nullptr) {
          continue;
        }

        d0 = sqrtf(d0);

        float
          radians = (2*M_PI*_player.GetFieldOfView())/360.0f;
        float 
          distort = cos(-radians/2.0f + (radians*i)/(float)SCREEN_WIDTH);
        int
          wall = (SCREEN_HEIGHT*(64.0f*SCALING_X)/SCALING_Y)/(d0*distort);
        float
          distance = (50.0f + 10.0f/(random_light + 1.0f))/d0;

        if (distance < 0.0f) {
          distance = 0.0f;
        } else if (distance > 1.0f) {
          distance = 1.0f;
        }

        if (_show_flat == true) {
          int
            color = 0xf0*distance;

          raster.SetColor(0xff000000 | color << 16 | color << 8 | color);
          raster.DrawLine({{i, SCREEN_HEIGHT/2 - wall}, {i, SCREEN_HEIGHT/2 + wall}});
        } else {
          jgui::Image 
            *texture = pbarrier->GetTexture();
          int 
            index = (int)(best.first*pbarrier->GetSize()*SCALING_Y)%64;

          for (int j=SCREEN_HEIGHT/2 - wall; j<SCREEN_HEIGHT/2 + wall; j++) {
            if (j > 0 and j < SCREEN_HEIGHT) {
              int size = j - SCREEN_HEIGHT/2 + wall;

              uint32_t
                pixel = texture->GetGraphics()->GetRGB({index, (64*size)/(2*wall)});
              uint8_t
                pr = (pixel >> 0x10) & 0xff,
                pg = (pixel >> 0x08) & 0xff,
                pb = (pixel >> 0x00) & 0xff;

              pr = pr*distance;
              pg = pg*distance;
              pb = pb*distance;

              raster.SetColor(0xff000000 | pr << 0x10 | pg << 0x08 | pb << 0x00);
              raster.SetPixel({i, j});
            }
          }

          // INFO:: floor with lights/shadows
          int wall_limit = SCREEN_HEIGHT/2 + wall;

          if (wall_limit > SCREEN_HEIGHT) {
            wall_limit = SCREEN_HEIGHT;
          }

          for (int j=wall_limit; j<SCREEN_HEIGHT; j++) {
            float
              d = SCREEN_HEIGHT/(2.0f*j - SCREEN_HEIGHT)/distort;
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

      _player.Paint(raster);

      if (_show_map == true) {
        raster.SetColor(0xffff0000);

				Ray 
					ray(_player.GetPosition(), _player.GetDirection());
        jgui::jpoint_t<float> 
          dir = ray.GetDirection();
        float 
          fov = 2*M_PI*_player.GetFieldOfView()/360.0,
          angle = -atan2f(-dir.y, -dir.x);

        jgui::Image 
          *rotate = _torch->Rotate(angle);

        float 
          arc0 = angle - fov/2 + M_PI,
          arc1 = angle + fov/2 + M_PI;

        raster.SetColor(0xff808080);
        raster.FillArc(ray.GetPosition(), {100, 100}, arc0, arc1);
        raster.DrawImage(rotate, {ray.GetPosition().x - rotate->GetSize().width/2, ray.GetPosition().y - rotate->GetSize().height/2});

        for (auto &barrier : _barriers) {
          barrier.Paint(raster);
        }
      }

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
