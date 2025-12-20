"use client"

import Image from 'next/image'
import { cn } from '@/lib/utils'
import type { GameType } from '@/lib/types'
import { GAME_TYPE_LABELS } from '@/lib/types'

interface GameTypeSelectorProps {
  selected: GameType | null
  onSelect: (gameType: GameType) => void
  className?: string
}

const gameTypes: { type: GameType; icon: string; emoji: string }[] = [
  { type: 'dominoes', icon: '/images/games/dominoes.png', emoji: '🁣' },
  { type: 'rummy', icon: '/images/games/rummy.png', emoji: '🃏' },
  { type: 'mahjong', icon: '/images/games/mahjong.png', emoji: '🀄' },
]

export function GameTypeSelector({
  selected,
  onSelect,
  className,
}: GameTypeSelectorProps) {
  return (
    <div className={cn("grid grid-cols-3 gap-4", className)}>
      {gameTypes.map(({ type, icon, emoji }) => (
        <button
          key={type}
          type="button"
          onClick={() => onSelect(type)}
          className={cn(
            "group relative flex flex-col items-center justify-center p-6 rounded-2xl border-2 transition-all duration-300",
            selected === type
              ? "border-[#ff2d75] bg-[#ff2d75]/10 shadow-[0_0_30px_rgba(255,45,117,0.3)]"
              : "border-white/10 bg-white/5 hover:border-[#00f0ff]/50 hover:bg-[#00f0ff]/5"
          )}
        >
          {/* Glow effect for selected */}
          {selected === type && (
            <div className="absolute inset-0 rounded-2xl bg-gradient-to-b from-[#ff2d75]/20 to-transparent pointer-events-none" />
          )}

          {/* Icon container */}
          <div className={cn(
            "relative w-16 h-16 mb-4 flex items-center justify-center text-5xl transition-transform duration-300",
            selected === type && "scale-110"
          )}>
            {emoji}
          </div>

          {/* Label */}
          <span
            className={cn(
              "font-semibold text-lg transition-all duration-300",
              selected === type
                ? "text-[#ff2d75] text-glow-pink"
                : "text-white/70 group-hover:text-white"
            )}
          >
            {GAME_TYPE_LABELS[type]}
          </span>

          {/* Selection indicator */}
          {selected === type && (
            <div className="absolute -top-1 -right-1 w-6 h-6 bg-[#ff2d75] rounded-full flex items-center justify-center shadow-[0_0_10px_rgba(255,45,117,0.5)]">
              <svg className="w-4 h-4 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={3} d="M5 13l4 4L19 7" />
              </svg>
            </div>
          )}
        </button>
      ))}
    </div>
  )
}
