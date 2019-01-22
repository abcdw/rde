// this is the style you want to emulate.
// This is the canonical layout file for the Quantum project. If you want to add another keyboard,

#include "atreus.h"

// Each layer gets a name for readability, which is then used in the keymap matrix below.
// The underscores don't mean anything - you can have a layer called STUFF or any other name.
// Layer names don't all need to be of the same length, obviously, and you can also skip them
// entirely and just use numbers.
#define _QW 0
#define _RS 1
#define _LW 2
#define _GC 3

#define _______ KC_TRNS

/*
MO(layer) - momentary switch to layer. As soon as you let go of the key, the layer is deactivated and you pop back out to the previous layer.
LT(layer, kc) - momentary switch to layer when held, and kc when tapped.
TG(layer) - toggles a layer on or off.
TO(layer) - Goes to a layer. This code is special, because it lets you go either up or down the stack -- just goes directly to the layer you want. So while other codes only let you go up the stack (from layer 0 to layer 3, for example), TO(2) is going to get you to layer 2, no matter where you activate it from -- even if you're currently on layer 5. This gets activated on keydown (as soon as the key is pressed).
TT(layer) - Layer Tap-Toggle. If you hold the key down, the layer becomes active, and then deactivates when you let go. And if you tap it, the layer simply becomes active (toggles on). It needs 5 taps by default, but you can set it by defining TAPPING_TOGGLE, for example, #define TAPPING_TOGGLE 2 for just two taps.
*/
#define TAPPING_TOGGLE 3


#define LC_BSPC     LCTL(KC_BSPC)
#define MC_SPC      LT(MO(_RS), KC_SPC)
/* #define MC_SPC      KC_SPC */
#define MC_TAB      ALT_T(KC_TAB)
#define MC_ENT      MT(MOD_RCTL, KC_ENT)
#define MC_ESC      CTL_T(KC_ESC)
#define MC_BSPC     SFT_T(KC_BSPC)
#define MC_BSP1     SFT_T(LC_BSPC)
#define MC_RCTL     RCTL_T(KC_0)
#define MC_LANG     LSFT(KC_LALT)
#define MC_Z        SFT_T(KC_Z)
#define MC_SLSH     SFT_T(KC_SLSH)
#define MC_DLR      SFT_T(KC_DLR)
#define MC_RBRC     SFT_T(KC_RBRC)

#define MC_BACK SEND_STRING("b"SS_LCTRL("6"));
/* #define OS_RCTL     MT(MOD_RCTL, OSM(MOD_RCTL)) */

/*
 *  `       [     {     }     ]        ||       \     7     8     9    /
 *  #     left    (     )   right      ||       *     4     5     6    -
 *  $       ?    down   up    &        ||       %     1     2     3    =
 *  !     insert super shift bksp ctrl || alt space   fn    v     0    ^
 */

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
[_QW] = { /* Qwerty */
  {KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_TRNS,  KC_Y,    KC_U,    KC_I,    KC_O,    KC_P    },
  {KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_TRNS,  KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN },
  {KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    MC_TAB,   KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH },
  {KC_EXLM, TG(_LW), KC_LGUI, MC_ESC,  MC_BSPC, MC_ENT,   KC_SPC,  MO(_RS), MC_LANG, KC_E,    KC_AT   }
},

/*
 *  !       @     up     {    }        ||     pgup    7     8     9    *
 *  #     left   down  right  $        ||     pgdn    4     5     6    +
 *  [       ]      (     )    &        ||       `     1     2     3    \
 * lower  insert super shift bksp ctrl || alt space   fn    .     0    =
 */

/* =-/+_? */
/* KC_LCBR */
/* !'!!@#**#!@' */
[_RS] = { /* [> RAISE <] */
  {KC_GRV,  KC_MINS, KC_UNDS, KC_PLUS, KC_EQL,  KC_TRNS, KC_BSLS, KC_7,    KC_8,    KC_9, KC_LBRC },
  {KC_HASH, KC_LPRN, KC_LEFT, KC_RGHT, KC_RPRN, KC_TRNS, KC_ASTR, KC_4,    KC_5,    KC_6, KC_QUOT },
  {KC_DLR,  KC_LCBR, KC_DOWN, KC_UP,   KC_AMPR, _______, KC_PERC, KC_1,    KC_2,    KC_3, KC_RBRC },
  {_______, TG(_LW), _______, _______, _______, _______, _______, _______, KC_0,    KC_E, KC_CIRC }
},

/*
 * insert home   up  end   pgup       ||      up     F7    F8    F9   F10
 *  del   left  down right pgdn       ||     down    F4    F5    F6   F11
 *       volup             reset      ||             F1    F2    F3   F12
 *       voldn  super shift bksp ctrl || alt space   L0  prtsc scroll pause
 */
[_LW] = { /* [> GAMING <] */
  {KC_1,    KC_2,     KC_3,     KC_4,    KC_B,    KC_TRNS, KC_F24,  KC_F7,   KC_F8,   KC_F9,   KC_F10  },
  {KC_Q,    KC_W,     KC_E,     KC_R,    KC_TAB,  KC_TRNS, KC_F22,  KC_F4,   KC_F5,   KC_F6,   KC_F11  },
  {KC_A,    KC_S,     KC_D,     KC_F,    KC_F24,  KC_P,    KC_DEL,  KC_F1,   KC_F2,   KC_F3,   KC_F12  },
  {KC_LSFT, KC_T,   KC_LALT,  KC_LCTL, KC_SPC,  MO(_GC),  KC_ESC,  TO(_QW), KC_PSCR, KC_SLCK, RESET }
},
[_GC] = { /* GAMING CHAT */
  {KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_TRNS,  KC_Y,    KC_U,    KC_I,    KC_O,    KC_P    },
  {KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_TRNS,  KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN },
  {KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    MC_TAB,   KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH },
  {KC_EXLM, TO(_LW), KC_LGUI, MC_ESC,  MC_BSPC, KC_ENT,   KC_SPC,  MO(_RS), MC_LANG, KC_E,    KC_AT   }
}

};

const uint16_t PROGMEM fn_actions[] = {

};

const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt)
{
  // MACRODOWN only works in this function
      switch(id) {
        case 0:
          if (record->event.pressed) {
            register_code(KC_RSFT);
          } else {
            unregister_code(KC_RSFT);
          }
        break;
      }
    return MACRO_NONE;
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  if (record->event.pressed) {
    switch(keycode) {
    case KC_F24:
      SEND_STRING("b"SS_LCTRL("6")SS_LCTRL("4"));
      /* SEND_STRING("b"); */
      return false; break;
    case KC_F23:
      SEND_STRING(SS_TAP(X_ENTER)",r"SS_TAP(X_ENTER));
      return false; break;
    case KC_F22:
      SEND_STRING(SS_LCTRL("3"));
      return false; break;
    }
  }
  return true;
};
