import React, { useState } from 'react';
import { ProjectStructure, GeneratedFile } from '../types';
import { Folder, FileCode, Download, Copy, Check, ChevronRight, ChevronDown } from 'lucide-react';

interface TransformationResultProps {
    project: ProjectStructure;
    onReset: () => void;
}

const TransformationResult: React.FC<TransformationResultProps> = ({ project, onReset }) => {
    const [selectedFile, setSelectedFile] = useState<GeneratedFile>(project.files[0]);
    const [copied, setCopied] = useState(false);

    const handleCopy = () => {
        navigator.clipboard.writeText(selectedFile.content);
        setCopied(true);
        setTimeout(() => setCopied(false), 2000);
    };

    // Simple sorting to group by folder loosely
    const sortedFiles = [...project.files].sort((a, b) => a.path.localeCompare(b.path));

    return (
        <div className="flex flex-col h-full max-w-6xl mx-auto">
            <div className="flex items-center justify-between mb-6">
                <div>
                    <h2 className="text-2xl font-bold text-slate-900">Transformation Complete</h2>
                    <p className="text-slate-500">Modern architecture generated successfully.</p>
                </div>
                <div className="flex gap-3">
                    <button 
                        onClick={onReset}
                        className="px-4 py-2 text-sm font-medium text-slate-600 bg-white border border-slate-200 rounded-lg hover:bg-slate-50"
                    >
                        Start Over
                    </button>
                    <button className="flex items-center gap-2 px-4 py-2 bg-indigo-600 text-white text-sm font-medium rounded-lg hover:bg-indigo-700 shadow-sm">
                        <Download className="w-4 h-4" />
                        Download Solution
                    </button>
                </div>
            </div>

            <div className="flex-1 bg-white border border-slate-200 rounded-xl shadow-sm overflow-hidden flex h-[600px]">
                {/* Sidebar - File Tree */}
                <div className="w-1/3 bg-slate-50 border-r border-slate-200 flex flex-col">
                    <div className="p-4 border-b border-slate-200 bg-slate-100/50">
                        <h3 className="text-xs font-semibold text-slate-500 uppercase tracking-wider">Solution Explorer</h3>
                    </div>
                    <div className="flex-1 overflow-y-auto p-2 space-y-0.5">
                        {sortedFiles.map((file) => {
                            const parts = file.path.split('/');
                            const fileName = parts.pop();
                            const folderPath = parts.join('/');
                            
                            return (
                                <button
                                    key={file.path}
                                    onClick={() => setSelectedFile(file)}
                                    className={`w-full text-left flex flex-col px-3 py-2 rounded-md text-sm transition-colors border border-transparent ${
                                        selectedFile.path === file.path 
                                            ? 'bg-white shadow-sm border-slate-200' 
                                            : 'hover:bg-slate-200/50'
                                    }`}
                                >
                                    {folderPath && (
                                        <span className="text-[10px] text-slate-400 font-mono mb-0.5 truncate w-full flex items-center gap-1">
                                            <Folder className="w-3 h-3" />
                                            {folderPath}
                                        </span>
                                    )}
                                    <div className={`flex items-center gap-2 ${selectedFile.path === file.path ? 'text-indigo-600 font-semibold' : 'text-slate-700'}`}>
                                        <FileCode className="w-4 h-4 shrink-0" />
                                        <span className="truncate" title={fileName}>
                                            {fileName}
                                        </span>
                                    </div>
                                </button>
                            );
                        })}
                    </div>
                </div>

                {/* Main Content - Code Editor */}
                <div className="w-2/3 flex flex-col bg-[#1e1e1e]">
                    <div className="flex items-center justify-between px-6 py-3 border-b border-slate-700 bg-[#252526]">
                        <div className="flex items-center gap-2 text-sm text-slate-300 font-mono">
                             <FileCode className="w-4 h-4 text-blue-400" />
                             {selectedFile.path}
                        </div>
                        <button 
                            onClick={handleCopy}
                            className="text-slate-400 hover:text-white transition-colors flex items-center gap-2 text-xs uppercase tracking-wider hover:bg-white/10 px-2 py-1 rounded"
                            title="Copy Code"
                        >
                            {copied ? <Check className="w-3 h-3 text-green-400" /> : <Copy className="w-3 h-3" />}
                            {copied ? 'Copied' : 'Copy'}
                        </button>
                    </div>
                    <div className="flex-1 overflow-auto p-6">
                        <pre className="font-mono text-sm text-[#d4d4d4] leading-relaxed">
                            <code>{selectedFile.content}</code>
                        </pre>
                    </div>
                </div>
            </div>
        </div>
    );
};

export default TransformationResult;