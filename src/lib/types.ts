// Core application types

export type GameType = 'dominoes' | 'rummy' | 'mahjong'

export type CompletionType = 'rounds' | 'points'

export type GameStatus = 'active' | 'completed' | 'cancelled'

export interface Player {
  id: string
  name: string
  nickname: string | null
  color: string
  avatar_url: string | null
  created_at: string
}

export interface Game {
  id: string
  game_type: GameType
  completion_type: CompletionType
  max_rounds: number | null
  max_points: number | null
  status: GameStatus
  winner_id: string | null
  started_at: string
  ended_at: string | null
}

export interface GamePlayer {
  id: string
  game_id: string
  player_id: string
  final_score: number
  player?: Player
}

export interface Score {
  id: string
  game_id: string
  player_id: string
  round: number
  score: number
  created_at: string
}

// Extended types for UI
export interface GameWithPlayers extends Game {
  game_players: (GamePlayer & { player: Player })[]
  winner?: Player
}

export interface PlayerScore {
  player: Player
  scores: number[]
  total: number
}

// Form types
export interface NewGameForm {
  gameType: GameType
  completionType: CompletionType
  maxRounds: number
  maxPoints: number
  playerIds: string[]
}

// Player presets (for initial migration)
export const INITIAL_PLAYERS: Omit<Player, 'id' | 'created_at'>[] = [
  { name: 'Gloria', nickname: 'Won Non', color: '#321D71', avatar_url: null },
  { name: 'Paul', nickname: 'Pah-OOL', color: '#8C1A10', avatar_url: null },
  { name: 'Lauren', nickname: 'Gerald McBoingBoing', color: '#48752C', avatar_url: null },
  { name: 'Craig', nickname: 'Ya Boii', color: '#2854C5', avatar_url: null },
  { name: 'Frank', nickname: 'Franconia Springfield', color: '#964B00', avatar_url: null },
  { name: 'Dave', nickname: 'Graham', color: '#000000', avatar_url: null },
  { name: 'Amy', nickname: 'Amy', color: '#CEA8BC', avatar_url: null },
  { name: 'Stacy', nickname: 'Stacy', color: '#7CA7D8', avatar_url: null },
  { name: 'Keith', nickname: 'Keith', color: '#0E2787', avatar_url: null },
]

export const GAME_TYPE_LABELS: Record<GameType, string> = {
  dominoes: 'Dominoes',
  rummy: 'Rummy',
  mahjong: 'Mahjong',
}

export const GAME_TYPE_ICONS: Record<GameType, string> = {
  dominoes: '/images/games/dominoes.png',
  rummy: '/images/games/rummy.png',
  mahjong: '/images/games/mahjong.png',
}
