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

const gameTypes: { type: GameType; icon: string; color: string }[] = [
  { type: 'dominoes', icon: '/images/dominoes.png', color: '#586F7C' },
  { type: 'rummy', icon: '/images/rummy.png', color: '#8B4513' },
  { type: 'mahjong', icon: '/images/mahjong.png', color: '#228B22' },
]

export function GameTypeSelector({
  selected,
  onSelect,
  className,
}: GameTypeSelectorProps) {
  return (
    <div className={cn("grid grid-cols-3 gap-4", className)}>
      {gameTypes.map(({ type, icon, color }) => (
        <button
          key={type}
          type="button"
          onClick={() => onSelect(type)}
          className={cn(
            "game-card flex flex-col items-center justify-center p-6 rounded-xl border-2 bg-card transition-all",
            selected === type
              ? "border-gn-gold neon-border"
              : "border-border hover:border-gn-gold/50"
          )}
        >
          <div className="relative w-16 h-16 mb-3">
            <Image
              src={icon}
              alt={GAME_TYPE_LABELS[type]}
              fill
              className="object-contain"
              onError={(e) => {
                // Fallback if image doesn't exist
                const target = e.target as HTMLImageElement
                target.style.display = 'none'
              }}
            />
            {/* Fallback icon */}
            <div className="absolute inset-0 flex items-center justify-center text-4xl">
              {type === 'dominoes' && '🁣'}
              {type === 'rummy' && '🃏'}
              {type === 'mahjong' && '🀄'}
            </div>
          </div>
          <span
            className={cn(
              "font-display text-lg font-semibold",
              selected === type ? "text-gn-gold" : "text-foreground"
            )}
          >
            {GAME_TYPE_LABELS[type]}
          </span>
        </button>
      ))}
    </div>
  )
}
