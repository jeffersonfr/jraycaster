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
#include "jcanvas/core/japplication.h"
#include "jcanvas/core/jwindow.h"
#include "jcanvas/core/jraster.h"
#include "jcanvas/core/jindexedimage.h"
#include "jcanvas/core/jbufferedimage.h"

#include <algorithm>
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

static uint32_t engine_clock = 0;

static uint8_t buffer[FIRE_SCREEN_HEIGHT * FIRE_SCREEN_WIDTH];

class Barrier {
private:
  std::shared_ptr<jcanvas::Image> mImage;
  jcanvas::jline_t<int> mLine;
  jcanvas::jpoint_t<int> mCenter;
  float mRadius;
  float mTextureScale;
  bool mIsLine;

public:
  Barrier(std::shared_ptr<jcanvas::Image> image, jcanvas::jpoint_t<int> p0, jcanvas::jpoint_t<int> p1)
    : mImage{std::move(image)}, mLine{p0, p1}, mCenter{}, mRadius(0), mTextureScale{1.0f}, mIsLine{true} {
  }

  Barrier(std::shared_ptr<jcanvas::Image> image, jcanvas::jpoint_t<int> p0, float radius)
    : mImage{std::move(image)}, mLine{}, mCenter{p0}, mRadius{radius}, mTextureScale{1.0f}, mIsLine{false} {
  }

  virtual ~Barrier() = default;

  std::optional<std::pair<float, jcanvas::jpoint_t<int> > > Intersection(jcanvas::jline_t<float> line) {
    if (mIsLine) {
      std::optional<std::pair<float, float> >
          tu = mLine.Intersection(line);

      if (tu == std::nullopt or tu->first < 0.0f or tu->first > 1.0f or tu->second < 0.0f) {
        return std::nullopt;
      }

      return std::make_pair(tu->first, mLine.Point(tu->first));
    }

    std::optional<std::pair<jcanvas::jpoint_t<float>, jcanvas::jpoint_t<float> > >
        points = jcanvas::jcircle_t<float>{mCenter, mRadius}.Intersection(line);

    if (points != std::nullopt) {
      float
          angle = (points->second - mCenter).Angle();

      // INFO:: hack to avoid a concave version of circle behind of player
      if ((line.p0 - points->first).Norm() > (line.p0 - points->second).Norm()) {
        return std::make_pair(angle / (2 * M_PI), points->second);
      }
    }

    return std::nullopt;
  }

  std::optional<float> Distance(jcanvas::jpoint_t<float> point) const {
    if (mIsLine) {
      std::optional<float>
          t = mLine.PerpendicularIntersection(point);

      if (t == std::nullopt or t < 0.0f or t > 1.0f) {
        return std::nullopt;
      }

      return mLine.Point(*t).Distance(point);
    }

    return mCenter.Distance(point) - mRadius;
  }

  [[nodiscard]] float GetSize() const {
    if (mIsLine) {
      return static_cast<float>(mLine.Size());
    }

    return M_PI * 2.0 * mRadius;
  }

  void Paint(jcanvas::Raster &raster) const {
    raster.SetColor(0xffff0000);

    if (mIsLine) {
      raster.DrawLine(mLine.p0, mLine.p1);

      return;
    }

    raster.DrawCircle(mCenter, mRadius);
  }

  void SetTexture(std::shared_ptr<jcanvas::Image> image) {
    mImage = image;
  }

  std::shared_ptr<jcanvas::Image> GetTexture() {
    return mImage;
  }

  void SetTextureScale(float param) {
    mTextureScale = 1.0f / param;

    if (mTextureScale < 0.0f) {
      mTextureScale = 0.0f;
    }
  }

  float GetTextureScale() const {
    return mTextureScale;
  }
};

class Sprite {
private:
  std::vector<std::shared_ptr<jcanvas::Image>> mFrames;
  jcanvas::jpoint_t<int> mPos{};
  jcanvas::jpoint_t<float> mDir{};
  int mIndex;
  float mOpacity;
  bool mAlive;

public:
  Sprite(std::shared_ptr<jcanvas::Image> image, int frames, jcanvas::jpoint_t<int> pos, jcanvas::jpoint_t<float> dir) {
    jcanvas::jpoint_t<int> size = image->GetSize();
    int step = size.x / frames;

    for (int i = 0; i < frames; i++) {
      mFrames.push_back(image->Crop({i * step, 0, step, size.y}));
    }

    mPos = pos;
    mDir = dir;
    mOpacity = 1.0f;
    mAlive = true;
    mIndex = random() % frames;
  }

  virtual ~Sprite() = default;

  void Invalidate() {
    mAlive = false;
  }

  [[nodiscard]] bool Valid() const {
    return mAlive;
  }

  [[nodiscard]] jcanvas::jpoint_t<int> GetPosition() const {
    return mPos;
  }

  std::shared_ptr<jcanvas::Image> GetTexture() {
    return mFrames[(mIndex + engine_clock / 2) % mFrames.size()];
  }

  void SetOpacity(float opacity) {
    mOpacity = opacity;
  }

  [[nodiscard]] float GetOpacity() const {
    return mOpacity;
  }

  void Update() {
    mPos += mDir;

    if (mPos.x < 0 or mPos.y < 0 or mPos.x > SCREEN_WIDTH or mPos.y > SCREEN_HEIGHT) {
      Invalidate();
    }
  }

  void Paint(jcanvas::Raster &raster) {
    raster.DrawImage(GetTexture()->Scale({16, 16}), {mPos.x - 8, mPos.y - 8});
  }
};

class Player {
private:
  std::vector<std::shared_ptr<jcanvas::Image>> mIdle;
  std::vector<std::shared_ptr<jcanvas::Image>> mFire;
  jcanvas::jpoint_t<int> mPos{};
  float mDir;
  float mFov;

public:
  explicit Player(float fov) {
    mPos = {
      0, 0
    };

    mDir = 0.0f;
    mFov = fov;

    auto image = std::make_shared<jcanvas::BufferedImage>("images/candle.png");
    auto image_fire = std::make_shared<jcanvas::BufferedImage>("images/candle-fire.png");
    jcanvas::jpoint_t<int> isize = image->GetSize();
    int frames = 4;
    int step = isize.x / frames;

    for (int i = 0; i < frames; i++) {
      mIdle.push_back(image->Crop({i * step, 0, step, step})->Scale({step * frames, step * frames}));
      mFire.push_back(image_fire->Crop({i * step, 0, step, step})->Scale({step * frames, step * frames}));
    }
  }

  virtual ~Player() = default;

  [[nodiscard]] float GetFieldOfView() const {
    return mFov;
  }

  void SetDirection(float dir) {
    mDir = dir;;
  }

  [[nodiscard]] float GetDirection() const {
    return mDir;
  }

  [[nodiscard]] jcanvas::jpoint_t<int> GetPosition() const {
    return mPos;
  }

  void SetPosition(jcanvas::jpoint_t<int> point) {
    mPos = point;
  }

  void LookAt(float angle) {
    mDir = mDir + angle;

    if (mDir < 0.0f) {
      mDir = mDir + 2.0f * M_PI;
    }

    mDir = fmod(mDir, 2.0f * M_PI);
  }

  void Step(int signal, float angle) {
    SetPosition(mPos + jcanvas::jpoint_t<float>{signal * PLAYER_STEP * cos(mDir + angle), signal * PLAYER_STEP * sin(mDir + angle)});
  }

  void Left() {
    Step(+1, -M_PI / 2.0f);
  }

  void Right() {
    Step(+1, M_PI / 2.0f);
  }

  void Forward() {
    Step(+1, 0.0f);
  }

  void Backward() {
    Step(-1, 0.0f);
  }

  void Paint(jcanvas::Raster &raster) const {
    static float
        arc = 0.0f;
    static int
        range = 32,
        pos = 0,
        dir = +1;

    arc = fmod(arc + 0.1, 2 * M_PI);
    pos = pos + dir * (random() % 3);

    if (pos < -range) {
      pos = -range;
      dir = 1;
    }

    if (pos > range) {
      pos = range;
      dir = -1;
    }

    int size = std::min(SCREEN_WIDTH, SCREEN_HEIGHT) / 2;

    raster.DrawImage(mIdle[(engine_clock / 2) % mIdle.size()], {(SCREEN_WIDTH - size) / 2 + pos, (int) (SCREEN_HEIGHT - 0.9f * size + 0.1f * size * sin(arc))});
  }
};

class Scene : public jcanvas::Window, public jcanvas::KeyListener {
private:
  std::map<std::string, std::shared_ptr<jcanvas::Image>> mImages;
  // jmedia::Player *mMedia;
  // jmedia::AudioMixerControl *mControl;
  // jmedia::Audio *mMusic;
  std::shared_ptr<jcanvas::Image> mScene;
  std::vector<Barrier> mBarriers;
  std::vector<Sprite> mSprites;
  Player mPlayer;
  float mZbuffer[SCREEN_WIDTH];
  bool mShowFlat{false};
  bool mShowMap{false};

public:
  Scene()
    : Window({SCREEN_WIDTH, SCREEN_HEIGHT}),  mPlayer(PLAYER_FOV)
  {
    mScene = std::make_shared<jcanvas::BufferedImage>(jcanvas::jpixelformat_t::RGB32, jcanvas::jpoint_t<int>{SCREEN_WIDTH, SCREEN_HEIGHT});

    mScene->GetGraphics()->SetAntialias(jcanvas::jantialias_t::None);

    mImages["splash"] = std::make_shared<jcanvas::BufferedImage>("images/splash.png");
    mImages["wall0"] = std::make_shared<jcanvas::BufferedImage>("images/greystone.png");
    mImages["wall1"] = std::make_shared<jcanvas::BufferedImage>("images/skulls.png");
    mImages["barrel"] = std::make_shared<jcanvas::BufferedImage>("images/barrel.png");
    mImages["ghost"] = std::make_shared<jcanvas::BufferedImage>("images/ghost.png");
    mImages["fireball"] = std::make_shared<jcanvas::BufferedImage>("images/fireball.png");
    mImages["player"] = std::make_shared<jcanvas::BufferedImage>("images/player.png");

    mBarriers.emplace_back(mImages["wall0"], jcanvas::jpoint_t<int>{0, 0}, jcanvas::jpoint_t<int>{SCREEN_WIDTH, 0});
    mBarriers.emplace_back(mImages["wall0"], jcanvas::jpoint_t<int>{SCREEN_WIDTH, 0}, jcanvas::jpoint_t<int>{SCREEN_WIDTH, SCREEN_HEIGHT});
    mBarriers.emplace_back(mImages["wall0"], jcanvas::jpoint_t<int>{SCREEN_WIDTH, SCREEN_HEIGHT}, jcanvas::jpoint_t<int>{0, SCREEN_HEIGHT});
    mBarriers.emplace_back(mImages["wall0"], jcanvas::jpoint_t<int>{0, SCREEN_HEIGHT}, jcanvas::jpoint_t<int>{0, 0});

    // circle barrier
    mBarriers.emplace_back(mImages["wall1"], jcanvas::jpoint_t<int>{(int) (random() % SCREEN_WIDTH), (int) (random() % SCREEN_HEIGHT)},
                           30);

    for (int i = 0; i < 3; i++) {
      mBarriers.emplace_back(mImages["wall1"],
                             jcanvas::jpoint_t<int>{(int) (random() % SCREEN_WIDTH), (int) (random() % SCREEN_HEIGHT)},
                             jcanvas::jpoint_t<int>{(int) (random() % SCREEN_WIDTH), (int) (random() % SCREEN_HEIGHT)});
    }

    for (int i = 0; i < 10; i++) {
      jcanvas::jpoint_t<int> pos = {(int) (random() % SCREEN_WIDTH), (int) (random() % SCREEN_HEIGHT)};

      mSprites.emplace_back(mImages["ghost"], 4, pos, jcanvas::jpoint_t<float>{0, 0});
      mSprites.rbegin()->SetOpacity(0.5f);
    }

    mPlayer.SetPosition(jcanvas::jpoint_t<int>{200, 250});

    // INFO:: fire
    srand(time(NULL));

    for (int j = 0; j < FIRE_SCREEN_HEIGHT; j++) {
      for (int i = 0; i < FIRE_SCREEN_WIDTH; i++) {
        buffer[j * FIRE_SCREEN_WIDTH + i] = 36;
      }
    }

    /*
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
    */
  }

  virtual ~Scene() {
    mBarriers.clear();

    /*
    if (_media != nullptr) {
      _media->Stop();
      _media->Close();
    }

    if (_music != nullptr) {
      delete _music;
      _music = nullptr;
    }
    */
  }

  virtual bool KeyPressed(jcanvas::KeyEvent *event) {
    if (event->GetSymbol() == jcanvas::jkeyevent_symbol_t::F1) {
      mShowMap = !mShowMap;
    } else if (event->GetSymbol() == jcanvas::jkeyevent_symbol_t::F2) {
      mShowFlat = !mShowFlat;
    }

    return true;
  }

  void KeyHandle() {
    static int fire_wait = 0;

    fire_wait = fire_wait - 1;

    jcanvas::EventManager &ev = GetEventManager();

    if (ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::Space)) {
      // INFO:: fire
      if (fire_wait < 0) {
        // TODO:: create an animated sprite with direction
        jcanvas::jpoint_t<float> dir{cos(mPlayer.GetDirection()), sin(mPlayer.GetDirection())};
        jcanvas::jpoint_t<int> pos = mPlayer.GetPosition() + dir * 50;

        mSprites.emplace_back(mImages["fireball"], 24, pos, dir * 25);

        fire_wait = 10;
      }
    }

    if (ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::CursorLeft)) {
      mPlayer.LookAt(-PLAYER_ROTATE);
    }

    if (ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::CursorRight)) {
      mPlayer.LookAt(PLAYER_ROTATE);
    }

    if (
      ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::CursorUp) or
      ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::CursorDown) or
      ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::w) or
      ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::s) or
      ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::a) or
      ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::d)) {
      jcanvas::jpoint_t<int> pos = mPlayer.GetPosition();

      if (ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::w) or ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::CursorUp)) {
        mPlayer.Forward();
      } else if (ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::s) or ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::CursorDown)) {
        mPlayer.Backward();
      } else if (ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::a)) {
        mPlayer.Left();
      } else if (ev.IsKeyDown(jcanvas::jkeyevent_symbol_t::d)) {
        mPlayer.Right();
      }

      for (auto barrier: mBarriers) {
        std::optional<float>
            distance = barrier.Distance(mPlayer.GetPosition());

        if (distance != std::nullopt and distance < 8.0f) {
          // INFO:: minumum perpendicular distance to wall
          mPlayer.SetPosition(pos);

          return;
        }
      }
    }
  }

  void Framerate(int fps) {
    static auto begin = std::chrono::steady_clock::now();
    static int index = 0;

    std::chrono::time_point<std::chrono::steady_clock> timestamp = begin + std::chrono::milliseconds(index++ * (1000 / fps));
    std::chrono::time_point<std::chrono::steady_clock> current = std::chrono::steady_clock::now();
    std::chrono::milliseconds diff = std::chrono::duration_cast<std::chrono::milliseconds>(timestamp - current);

    if (diff.count() < 0) {
      return;
    }

    std::this_thread::sleep_for(diff);
  }

  void PaintSprites(jcanvas::Raster &raster) {
    // INFO:: draw sprites
    jcanvas::jpoint_t<int> ppos = mPlayer.GetPosition();

    std::sort(mSprites.begin(), mSprites.end(), [&](Sprite &s1, Sprite &s2) {
      return ppos.Distance(s1.GetPosition()) > ppos.Distance(s2.GetPosition());
    });

    for (auto &sprite: mSprites) {
      jcanvas::jpoint_t<int> dpos = sprite.GetPosition() - ppos;
      float object_angle = atanf(dpos.y / (float) dpos.x) - mPlayer.GetDirection();

      if (dpos.x < 0) {
        object_angle = object_angle + M_PI;
      }

      object_angle = fmod(object_angle + mPlayer.GetFieldOfView() / 2, 2.0f * M_PI);

      if (object_angle < 0.0f) {
        object_angle = object_angle + 2.0f * M_PI;
      }

      object_angle = object_angle - mPlayer.GetFieldOfView() / 2.0f;

      bool inside_player_view = fabs(object_angle) < ((mPlayer.GetFieldOfView() + M_PI / 6.0f) / 2.0f);
      // OPTIMIZE:: process only sprites in field of view (increase field of view to capture unbounded sprites)

      if (inside_player_view) {
        std::shared_ptr<jcanvas::Image> image = sprite.GetTexture();
        jcanvas::jpoint_t<int> size = image->GetSize();

        float
            object_distance = dpos.EuclidianNorm(),
            object_ceiling = SCREEN_HEIGHT / 2.0f - SCREEN_HEIGHT / (2.0f * object_distance),
            object_floor = SCREEN_HEIGHT - object_ceiling,
            object_height = (object_floor - object_ceiling) * size.y,
            object_ratio = size.y / (float) size.x,
            object_width = object_height / object_ratio,
            object_center = (0.5f * (object_angle / (mPlayer.GetFieldOfView() / 2.0f)) + 0.5f) * SCREEN_WIDTH;

        for (int j = 0; j < object_height; j++) {
          int y = object_ceiling + j;

          if (y < 0) {
            continue;
          }

          if (y >= SCREEN_HEIGHT) {
            break;
          }

          for (int i = 0; i < object_width; i++) {
            float sample_x = i / object_width;
            float sample_y = j / object_height;
            int x = (int) (object_center + i - object_width / 2.0f);

            if (x < 0) {
              continue;
            }

            if (x >= SCREEN_WIDTH) {
              break;
            }

            if (object_distance <= mZbuffer[x]) {
              uint32_t pixel = image->GetGraphics()->GetRGB({(int) (sample_x * size.x), (int) (sample_y * size.y)});

              if (pixel & 0xff000000) {
                float light = 1.0f - object_distance / std::max(SCREEN_WIDTH, SCREEN_HEIGHT);
                uint8_t
                    pr = (pixel >> 0x10) & 0xff,
                    pg = (pixel >> 0x08) & 0xff,
                    pb = (pixel >> 0x00) & 0xff;

                pr = pr * light;
                pg = pg * light;
                pb = pb * light;

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
    mSprites.erase(std::remove_if(mSprites.begin(), mSprites.end(),
      [](Sprite &param) {
        return param.Valid() == false;
    }), mSprites.end());
  }

  void PaintFire(jcanvas::Raster &raster) {
    for (int j = 0; j < FIRE_SCREEN_HEIGHT - 1; j++) {
      for (int i = 0; i < FIRE_SCREEN_WIDTH; i++) {
        int decay = random() % 3;
        int intensity = buffer[(j + 1) * FIRE_SCREEN_WIDTH + i] - decay;

        if (intensity < 0) {
          intensity = 0;
        }

        buffer[j * FIRE_SCREEN_WIDTH + (i + decay)] = intensity;
      }
    }

    std::shared_ptr<jcanvas::Image> image = std::make_shared<jcanvas::IndexedImage>(palette, 37, (uint8_t *) buffer, jcanvas::jpoint_t<int>{FIRE_SCREEN_WIDTH, FIRE_SCREEN_HEIGHT});

    raster.DrawImage(image->Crop({(int) (FIRE_SCREEN_WIDTH * mPlayer.GetDirection() / (2.0f * M_PI)), 0, FIRE_SCREEN_WIDTH / 3, FIRE_SCREEN_HEIGHT})->Scale({SCREEN_WIDTH, SCREEN_HEIGHT / 2}), {0, 0});
  }

  void PaintPlayer(jcanvas::Raster &raster) {
    mPlayer.Paint(raster);
  }

  void PaintMap(jcanvas::Raster &raster) {
    std::shared_ptr<jcanvas::Image> rotate = mImages["player"]->Rotate(-mPlayer.GetDirection() + M_PI);
    jcanvas::jpoint_t<int> pos = mPlayer.GetPosition();
    jcanvas::jpoint_t<int> size = rotate->GetSize();
    float arc0 = -mPlayer.GetDirection() + mPlayer.GetFieldOfView() / 2.0f;
    float arc1 = -mPlayer.GetDirection() - mPlayer.GetFieldOfView() / 2.0f;
    int color = 0x80 + random() % 64 - 32;

    raster.SetColor(0xff000000 | color << 0x10 | color << 0x08 | color);
    raster.FillArc(pos, {100, 100}, arc1, arc0);
    raster.DrawImage(rotate, {pos.x - size.x / 2, pos.y - size.y / 2});

    for (auto &barrier: mBarriers) {
      barrier.Paint(raster);
    }

    for (auto &sprite: mSprites) {
      sprite.Paint(raster);
    }
  }

  void Paint(jcanvas::Graphics *g) {
    jcanvas::Raster raster((uint32_t *) cairo_image_surface_get_data(mScene->GetGraphics()->GetCairoSurface()), mScene->GetSize());

    raster.Clear();

    PaintFire(raster);

    // INFO:: key handling
    KeyHandle();

    // INFO:: draw walls
    jcanvas::jpoint_t<int> pos = mPlayer.GetPosition();
    int random_light = random() % 10;

    // INFO:: 3d map
    for (int i = 0; i < SCREEN_WIDTH; i++) {
      std::pair<float, jcanvas::jpoint_t<int>> best = {-1.0f, {9999, 9999}};
      Barrier *pbarrier = nullptr;
      float angle = -mPlayer.GetFieldOfView() / 2.0f + i * mPlayer.GetFieldOfView() / SCREEN_WIDTH + mPlayer.GetDirection();
      float d0 = (pos - best.second).Norm();

      // circle
      for (auto &barrier: mBarriers) {
        jcanvas::jline_t<float> ray = {pos, jcanvas::jpoint_t<float>(pos) + jcanvas::jpoint_t<float>{cos(angle), sin(angle)}};
        std::optional<std::pair<float, jcanvas::jpoint_t<int>>> intersection = barrier.Intersection(ray);

        if (intersection != std::nullopt) {
          float d1 = (pos - intersection->second).Norm();

          if (d1 < d0) {
            best = *intersection;
            d0 = d1;
            pbarrier = &barrier;
          }
        }
      }

      if (pbarrier == nullptr) {
        continue;
      }

      d0 = sqrtf(d0);

      mZbuffer[i] = d0;

      float cosf = cos(-mPlayer.GetFieldOfView() / 2.0f + (i * mPlayer.GetFieldOfView()) / (float) SCREEN_WIDTH);
      int wall = (SCREEN_HEIGHT * SCALING_MAP) / (d0 * cosf);

      if (mShowFlat == true) {
        raster.SetColor(0xfff0f0f0);
        raster.DrawLine({i, SCREEN_HEIGHT / 2 - wall}, {i, SCREEN_HEIGHT / 2 + wall});
      } else {
        std::shared_ptr<jcanvas::Image> texture = pbarrier->GetTexture();
        jcanvas::jpoint_t<int> tsize = texture->GetSize();
        float scale = pbarrier->GetTextureScale();
        int index = (int) (best.first * tsize.x);

        if (scale != 0.0f) {
          index = (int) (best.first * pbarrier->GetSize() * scale) % tsize.x;
        }

        // INFO:: casting walls
        for (int j = SCREEN_HEIGHT / 2 - wall; j < SCREEN_HEIGHT / 2 + wall; j++) {
          if (j > 0 and j < SCREEN_HEIGHT) {
            int size = j - SCREEN_HEIGHT / 2 + wall;

            raster.SetColor(texture->GetGraphics()->GetRGB({index, (tsize.y * size) / (2 * wall)}));
            raster.SetPixel({i, j});
          }
        }

        // INFO:: floor with lights/shadows
        int wall_limit = SCREEN_HEIGHT / 2 + wall;

        if (wall_limit > SCREEN_HEIGHT) {
          wall_limit = SCREEN_HEIGHT;
        }

        // fire light on floor
        for (int j = wall_limit; j < SCREEN_HEIGHT; j++) {
          float d = SCREEN_HEIGHT / (2.0f * j - SCREEN_HEIGHT) / cosf;
          int c = 0xff - (0x80 + random_light) * d;

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

    if (mShowMap == true) {
      PaintMap(raster);
    }

    // INFO:: splash screen
    static int splash_timer = 0;

    if (splash_timer++ < 100) {
      mScene->GetGraphics()->DrawImage(mImages["splash"], {0, 0, mScene->GetSize()});
    }

    g->DrawImage(mScene, {{0, 0}, GetSize()});

    engine_clock++;

    Repaint();

    Framerate(25);
  }
};

int main(int argc, char **argv) {
  jcanvas::Application::Init(argc, argv);

  srandom(time(NULL));

  Scene app;

  app.SetTitle("Scene");
  app.Exec();

  jcanvas::Application::Loop();

  return 0;
}
