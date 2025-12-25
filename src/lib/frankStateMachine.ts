/**
 * Frank State Machine - Maps game events to animations
 */

export type FrankAction =
  | 'dealing'
  | 'idle-watching'
  | 'celebrating'
  | 'sympathetic'
  | 'shuffling'
  | 'waving'
  | 'smoking'
  | 'pointing'
  | 'thinking'
  | 'excited'
  | 'confetti'
  | 'card-tricks'
  | 'drumroll'
  | 'nodding'
  | 'shrugging'

export type FrankMood = 'neutral' | 'excited' | 'sad'

export type FrankEvent =
  | 'page_load'
  | 'game_start'
  | 'game_win'
  | 'game_lose'
  | 'high_score'
  | 'low_score'
  | 'negative_score'
  | 'close_game'
  | 'round_end'
  | 'hot_streak'
  | 'cold_streak'
  | 'tie_game'
  | 'loading'
  | 'confirmation'
  | 'error'
  | 'medium_score_point'    // NEW - triggers pointing
  | 'medium_score_nod'      // NEW - triggers nodding
  | 'random_shrug'          // NEW - random shrugging
  | 'idle_cycle'            // NEW - for idle animation rotation

export interface FrankState {
  action: FrankAction
  mood: FrankMood
  dialogue: string | null
  showDialogue: boolean
  priority: number // Higher priority animations interrupt lower ones
}

export interface GameContext {
  gameType?: 'dominoes' | 'rummy' | 'mahjong'
  playerName?: string
  score?: number
  totalScore?: number
  scoreDiff?: number // Difference between top 2 players
  roundNumber?: number
  lastThreeScores?: number[]
  isWinner?: boolean
}

// Priority levels: 0 = can be interrupted, 3 = highest priority
const eventPriority: Record<FrankEvent, number> = {
  game_win: 3,
  game_lose: 3,
  hot_streak: 2,
  high_score: 2,
  round_end: 2,
  close_game: 2,
  low_score: 1,
  negative_score: 1,
  cold_streak: 1,
  tie_game: 1,
  confirmation: 1,
  game_start: 1,
  medium_score_point: 1,  // NEW
  medium_score_nod: 1,    // NEW
  random_shrug: 1,        // NEW
  loading: 0,
  page_load: 0,
  error: 0,
  idle_cycle: 0,          // NEW
}

/**
 * Determines Frank's state based on a game event and context
 */
export function getFrankStateForEvent(
  event: FrankEvent,
  context: GameContext = {}
): FrankState {
  const priority = eventPriority[event]

  switch (event) {
    case 'game_win':
      return {
        action: context.isWinner ? 'confetti' : 'sympathetic',
        mood: context.isWinner ? 'excited' : 'sad',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'high_score':
      return {
        action: 'celebrating',
        mood: 'excited',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'low_score':
    case 'negative_score':
      return {
        action: 'sympathetic',
        mood: 'sad',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'close_game':
      return {
        action: 'excited',
        mood: 'excited',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'round_end':
      return {
        action: 'drumroll',
        mood: 'neutral',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'hot_streak':
      return {
        action: 'card-tricks',
        mood: 'excited',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'tie_game':
      return {
        action: 'shrugging',
        mood: 'neutral',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'confirmation':
      return {
        action: 'nodding',
        mood: 'neutral',
        dialogue: null,
        showDialogue: false,
        priority,
      }

    case 'loading':
      return {
        action: 'dealing',
        mood: 'neutral',
        dialogue: null,
        showDialogue: false,
        priority,
      }

    case 'game_start':
      return {
        action: 'shuffling',
        mood: 'neutral',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'medium_score_point':
      return {
        action: 'pointing',
        mood: 'neutral',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'medium_score_nod':
      return {
        action: 'nodding',
        mood: 'neutral',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'random_shrug':
      return {
        action: 'shrugging',
        mood: 'neutral',
        dialogue: null,
        showDialogue: true,
        priority,
      }

    case 'idle_cycle':
      // Rotate between idle animations
      const idleAnimations: FrankAction[] = ['idle-watching', 'smoking', 'thinking']
      const randomIdle = idleAnimations[Math.floor(Math.random() * idleAnimations.length)]
      return {
        action: randomIdle,
        mood: 'neutral',
        dialogue: null,
        showDialogue: false,  // No dialogue during idle cycling
        priority,
      }

    default:
      return {
        action: 'idle-watching',
        mood: 'neutral',
        dialogue: null,
        showDialogue: false,
        priority: 0,
      }
  }
}

/**
 * Analyzes score to determine if it triggers an event
 */
export function detectScoreEvent(
  score: number,
  context: GameContext
): FrankEvent | null {
  // High score threshold (lowered from 30 to 10)
  if (score > 10) {
    return 'high_score'
  }

  // Medium score (new) - triggers pointing or nodding
  if (score >= 5 && score <= 10) {
    // Randomly use pointing or nodding for medium scores
    return Math.random() > 0.5 ? 'medium_score_point' : 'medium_score_nod'
  }

  // Low but positive score (changed from 1-9 to 0-4)
  if (score >= 0 && score < 5) {
    return 'low_score'
  }

  // Negative score
  if (score < 0) {
    return 'negative_score'
  }

  // Check for hot streak (lowered from 3 scores >20 to 2 scores >10)
  if (context.lastThreeScores && context.lastThreeScores.length >= 2) {
    const recentScores = context.lastThreeScores.slice(-2) // Get last 2 scores
    if (recentScores.every((s) => s > 10)) {
      return 'hot_streak'
    }
  }

  return null
}

/**
 * Analyzes game state to determine if close game
 */
export function detectCloseGame(
  topScore: number,
  secondScore: number
): FrankEvent | null {
  const diff = topScore - secondScore
  const percentDiff = topScore > 0 ? diff / topScore : 0

  if (percentDiff < 0.2) {
    // Less than 20% difference (lowered from 10% for more frequent excited animations)
    return 'close_game'
  }

  return null
}

/**
 * Gets page-specific default Frank state
 */
export function getDefaultStateForRoute(pathname: string): FrankState {
  if (pathname === '/') {
    return {
      action: 'waving',
      mood: 'neutral',
      dialogue: null,
      showDialogue: true, // Frank greets on home page
      priority: 0,
    }
  }

  if (pathname === '/play') {
    return {
      action: 'shuffling',
      mood: 'neutral',
      dialogue: null,
      showDialogue: true, // Frank welcomes on play page
      priority: 0,
    }
  }

  if (pathname === '/players') {
    return {
      action: 'waving',
      mood: 'neutral',
      dialogue: null,
      showDialogue: true, // Frank greets on players page
      priority: 0,
    }
  }

  if (pathname.startsWith('/game/')) {
    return {
      action: 'idle-watching',
      mood: 'neutral',
      dialogue: null,
      showDialogue: true, // Frank watches and comments during games
      priority: 0,
    }
  }

  // Default fallback
  return {
    action: 'smoking',
    mood: 'neutral',
    dialogue: null,
    showDialogue: true, // Frank shows dialogue by default
    priority: 0,
  }
}
